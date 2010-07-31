/* sxr -- Scala X-Ray
 * Copyright 2009, 2010 Mark Harrah, Olivier Michallat
 */

package sxr

import scala.tools.nsc.{ast, plugins, symtab, util, Global}
import ast.parser.Tokens
import plugins.Plugin
import symtab.Flags
import util.SourceFile

import java.io.{File, Reader, Writer}
import java.net.URL
import forScope._

import OutputFormat.{OutputFormat, getWriter}
import Browse._

object Browse
{
	/** The path to a link index, which allows linking between runs.  This applies for both incremental and remote linking.*/
	val LinkIndexRelativePath = "link.index"
	/** The path to a compressed link index, which allows linking between runs.  This applies for both incremental and remote linking.*/
	val CompressedLinkIndexRelativePath = LinkIndexRelativePath + ".gz"
	/** The name of the directory containing cached remote `link.index`es*/
	val CacheRelativePath = "cache.sxr"
}

/** The actual work extracting symbols and types is done here. */
abstract class Browse extends Plugin
{
	def classDirectory: File
	/** The output formats to write */
	def outputFormats: List[OutputFormat]
	/** The URLs of external sxr locations */
	def externalLinkURLs: List[URL]
	/** Relativizes the path to the given Scala source file against the base directories. */
	def getRelativeSourcePath(source: File): String
	/** The compiler.*/
	val global: Global

	import global._

	lazy val outputDirectory = new File(classDirectory.getParentFile, classDirectory.getName + ".sxr")
	outputDirectory.mkdirs

	private val linkIndexFile = new File(outputDirectory, LinkIndexRelativePath)
	private def linkStore = new LinkMapStore(linkIndexFile)

	/** The entry method for invoking the configured writers to generate the output.*/
	def generateOutput(externalLinks: List[LinkMap])
	{
		val sourceFiles = currentRun.units.map(getSourceFile(_))

		val links = new CompoundLinkMap(linkStore.read(None), externalLinks)
		links.clear(sourceFiles.map(getRelativeSourcePath(_)).toList)

		val context = new OutputWriterContext(sourceFiles, outputDirectory, settings.encoding.value, externalLinkURLs)
		val writers = outputFormats.map(getWriter(_, context))
		writers.foreach(_.writeStart())

		for(unit <- currentRun.units)
		{
			val sourceFile = getSourceFile(unit)

			// generate the tokens
			val tokens = scan(unit)
			val traverser = new Traverse(tokens, unit.source, links)
			traverser(unit.body)
			val tokenList = tokens.toList
			Collapse(tokenList, links)

			writers.foreach(_.writeUnit(sourceFile, getRelativeSourcePath(sourceFile), tokenList))
		}
		writers.foreach(_.writeEnd())
		linkStore.write(links)
	}
	private def getSourceFile(unit: CompilationUnit) = unit.source.file.file.getAbsoluteFile
	/** Tokenizes the given source.  The tokens are put into an ordered set by the start position of the token.
	* Symbols will be mapped back to these tokens by the offset of the symbol.*/
	private def scan(unit: CompilationUnit) =
	{
		val tokens = wrap.Wrappers.treeSet[Token]
		val scanner = new syntaxAnalyzer.UnitScanner(unit) { override def init {}; def parentInit = super.init }
		implicit def iterator28(s: syntaxAnalyzer.UnitScanner) = 
		{
			class CompatIterator extends Iterator[(Int, Int, Int)]
			{
				def next =
				{
						type TD = { def offset: Int; def lastOffset: Int; def token: Int }
						class Compat { def prev: TD = null; def next: TD = null; def offset = 0; def token = 0; def lastOffset = 0 }
						implicit def keep27SourceCompatibility(a: AnyRef): Compat =  new Compat// won't ever be called
					val offset = s.offset
					val token = s.token
					s.nextToken
					(offset, (s.lastOffset - offset) max 1, token)
				}
				def hasNext = s.token != Tokens.EOF
			}
			
			scanner.parentInit
			new { def iterator = new CompatIterator }
		}
		for( (offset, length, code) <- scanner.iterator)
		{
			if(includeToken(code))
				tokens += new Token(offset, length, code)
		}

		// Comment handling:
		// in 2.7 comments are included in the token stream
		// in 2.8 comments are collected separately in unit.comments
		
		// Compability for 2.7: return no comments with the syntax of 2.8
		import unit._
		implicit def noCommentsInScala27(u: CompilationUnit) = new {
			def comments: Seq[Comment] = Seq.empty
		}
		implicit def rangePositionNeedsStartEndIn27(r: util.RangePosition) = new {
			def start = 0
			def end = 0
		}
		
	        for (Comment(_, pos) <- unit.comments)
		  tokens += new Token(pos.start, pos.end - pos.start + 1, Tokens.COMMENT)

		tokens
	}
	/** Filters out unwanted tokens such as whitespace and commas.  Braces are currently
	* included because () is annotated as Unit, and a partial function created by
	* { case ... } is associated with the opening brace.  */
	private def includeToken(code: Int) =
	{
		import Tokens.{COMMENT, USCORE, isBrace, isKeyword, isIdentifier, isLiteral}
		code match
		{
			case COMMENT | USCORE => true
			case _ => isKeyword(code) || isIdentifier(code) || isLiteral(code) || isBrace(code)
		}
	}
	/** Gets the token for the given offset.*/
	private def tokenAt(tokens: wrap.SortedSetWrapper[Token], offset: Int): Option[Token] =
	{
		// create artificial tokens to get a subset of the tokens starting at the given offset
		// then, take the first token in the range
		tokens.range(new Token(offset, 1, 0), new Token(offset+1, 1, 0)).first
	}

	/** Filters unwanted symbols, such as packages.*/
	private def ignore(s: Symbol): Boolean =
		ignoreBase(s) ||
		s.isModuleClass || // ignore the generated class for modules
		s.isPrimaryConstructor // the primary constructor overlaps with the class type, so just use the class type
	private def ignoreBase(s: Symbol): Boolean =
		!s.exists ||
		s.isPackage || // nothing done with packages
		s.isImplClass
		
	private class Traverse(tokens: wrap.SortedSetWrapper[Token], source: SourceFile, links: LinkMap) extends Traverser
	{
		// magic method #1
		override def traverse(tree: Tree)
		{
			def handleDefault()
			{
				process(tree)
				super.traverse(tree)
			}
			tree match
			{
				case ValDef(_, _, _, rhs) =>
					// tests for synthetic val created for the x in x :: Nil, which would associate the wrong type with ::
					// because the synthetic val is associated with the :: token
					if(tree.symbol != null && tree.symbol.hasFlag(Flags.SYNTHETIC))
						traverse(rhs)
					else
						handleDefault()
				case Template(parents, self, body) =>
					// If the first parent in the source is a trait, the first parent in parents will be AnyRef and it will
					// use the trait's token, bumping the trait.  So, this hack processes traits first
					val (traits, notTraits) = parents.partition(_.symbol.isTrait)
					traverseTrees(traits)
					traverseTrees(notTraits)
					if (!self.isEmpty) traverse(self)
					traverseStats(body, tree.symbol)
				/*case DefDef(_, _, tparams, vparamss, _, rhs) =>
					atOwner(tree.symbol) {
						traverseTrees(tparams); traverseTreess(vparamss); traverse(rhs)
					}*/
				case _ =>
					handleDefault()
			}
		}
		
		// magic method #2
		private def process(t: Tree)
		{
			// this implicit exists for 2.7/2.8 compatibility
			implicit def source2Option(s: SourceFile): Option[SourceFile] = Some(s)
			def catchToNone[T](f: => Option[T]): Option[T] = try { f } catch { case e: UnsupportedOperationException => None }
			for(tSource <- catchToNone(t.pos.source) if tSource == source; offset <- t.pos.offset; token <- tokenAt(tokens, offset))
			{
				def processDefaultSymbol() =
				{
					if(t.hasSymbol && !ignore(t.symbol))
						processSymbol(t, token, source.file.file, links)
				}
				def processSimple() { token.tpe = TypeAttribute(typeString(t.tpe), None) }
				def processTypeTree(tt: TypeTree) { if(!ignore(tt.symbol)) processSymbol(tt, token, source.file.file, links) }
				t match
				{
					case _: ClassDef => processDefaultSymbol()
					case _: ModuleDef => processDefaultSymbol()
					case _: ValOrDefDef => processDefaultSymbol()
					case _: TypeDef => processDefaultSymbol()
					//case _: Super => processDefaultSymbol()
					case _: This => processDefaultSymbol()
					case s: Select => processDefaultSymbol()
					case _: New => processSimple()
					case _: Sequence => processDefaultSymbol()
					case _: Alternative => processDefaultSymbol()
					case _: Star => processDefaultSymbol()
					case _: Bind => processDefaultSymbol()
					case Apply(fun, args) =>
						/*val funInfo = fun.symbol.info
						println("Function: " + fun + "  " + funInfo + "  " + funInfo.getClass)
						funInfo match
						{
							case PolyType(tparams, MethodType(params, resultType) =>
								println("PolyType method type: " params)
							case MethodType(params, resultType) =>
								println("MethodType method type: " params)
						}
						println("Args: " + args.getClass + " " + args.map(_.getClass))
						*/
					 //processDefaultSymbol()
						/*fun match
						{
							case tt: TypeTree => if(!ignoreBase(tt.symbol)) processTypeTree(tt)
							case _ => traverse(fun)
						}
						traverseTrees(args)*/
					//case _: Import => processSimple()
					case _: Return => processSimple()
					case _: If => processSimple()
					case _: Match => processSimple() // this will annotate the 'match' keyword with the type returned by the associated pattern match
					case _: CaseDef => processSimple() // this will annotate the 'case' keyword with the type returned by that particular case statement
					case _: Throw => processSimple()
					case ta: TypeApply => processSimple() // this fills in type parameters for methods
					case Ident(_) => processDefaultSymbol()
					case Literal(value) => processSimple() // annotate literals
					case tt: TypeTree =>
						if(token.isPlain)
							processSymbol(tt, token, source.file.file, links)
					case _ => ()
				}
			}
		}
	}
		// magic method #3
	private def processSymbol(t: Tree, token: Token, sourceFile: File, links: LinkMap)
	{
		val sym = t.symbol
		def addDefinition()
		{
			val id = sym.id
			if(publicSymbol(sym)) {
				val stable = stableID(sym)
				val source = getRelativeSourcePath(sourceFile)
				links(source, stable) = id
				token += stable
				token.source = source
			}
			token += id
		}
		sym match
		{
			case ts: TermSymbol =>
				val sType =
					t match
					{
						case ad: ApplyDynamic => ad.qual.tpe.memberType(ad.symbol)
						case s: Select => s.qualifier.tpe.memberType(s.symbol)
						case _ => ts.owner.thisType.memberType(ts)
					}
				if(sType != null)
				{
					val asString =
						sType match
						{
							case mt: MethodType if ts.hasFlag(Flags.IMPLICIT)=> "implicit " + fullName(sym) + " : " + typeString(sType)
							case _ => typeString(sType)
						}
					//println("Term symbol " + sym.id + ": " + asString)
					token.tpe = TypeAttribute(asString, linkTo(sourceFile, sType.typeSymbol, links))
				}
			case ts: TypeSymbol =>
				val treeType = t.tpe
				val sType =
					if(treeType == NoType) ts.info
					else treeType
				//println("Type symbol " + sym.id + ": " + typeString(sType))
				if(sType != null)
					token.tpe = TypeAttribute(typeString(sType), linkTo(sourceFile, sType.typeSymbol, links))
			case _ => ()
		}
		if(sym != null && sym != NoSymbol)
		{
			if(t.isDef)
				addDefinition()
			else
			{
				linkTo(sourceFile, sym, links) match
				{
					case Some(x) => token.reference = x
					case None => ()//addDefinition()
				}
			}
		}
	}
	private def publicSymbol(s: Symbol): Boolean =
		!s.isLocal
	/** Constructs a decoded fully qualified name for the given symbol. */
	private def fullName(s: Symbol): String =
	{
		require(s != NoSymbol)
		val owner = s.owner
		require(owner != NoSymbol)
		if(owner.isRoot || owner.isEmptyPackageClass)
			s.nameString
		else
			fullName(owner.enclClass) + "." + s.nameString
	}
	/** Produces a string for the given type that should be informative, but brief.*/
	private def typeString(t: Type): String =
	{
		t match
		{
			case ct: CompoundType => compoundTypeString(ct, "")// tries to reduce size of some type strings
			case pt: PolyType =>
				import pt._
				if(typeParams.isEmpty)
					"=> " + typeString(resultType)
				else
				{
					val typeParameters = typeParams.map(_.defString).mkString("[", ", ", "]")
					resultType match
					{
						case ct: CompoundType => compoundTypeString(ct, typeParameters)
						case _ => typeParameters + typeString(resultType)
					}
				}
			case _ =>
				if(t == null)
					""
				else
					t.toString
		}
	}
	/** Converts the given compound type to a string.  'mainPostfix' is copied after the main type symbol
	* but before any parents or refinements*/
	private def compoundTypeString(ct: CompoundType, mainPostfix: String) =
	{
		import ct._
		typeSymbol.toString + mainPostfix +
		{
			if(ct.typeSymbol.isPackageClass)
				""
			else if(parents.isEmpty)
			{
				if(decls.isEmpty)
					""
				else
					decls.mkString("{", "; ", "}")
			}
			else
				parents.mkString(" extends ", " with ", "")
		}
	}

	/** Generates a link usable in the file 'from' to the symbol 'to', which might be in some other file. */
	private def linkTo(from: File, sym: Symbol, links: LinkMap): Option[Link] =
	{
		if(sym == null || sym == NoSymbol || sym.owner == NoSymbol)
			None
		else
		{
			val source = sym.sourceFile
			if(source == null)
				externalLinkTo(from, sym, links)
			else
				makeLink(from, source.file, sym.id)
		}
	}
	private def makeLink(from: File, to: File, id: Int): Some[Link] =
	{
		val base = if(to == from) "" else FileUtil.relativePath(relativeSource(from), relativeSource(to))
		Some(new Link(base, id, None))
	}
	private def externalLinkTo(from: File, sym: Symbol, links: LinkMap): Option[Link] =
	{
		if(publicSymbol(sym))
		{
			val name = stableID(sym)
			links(name) map { case (src, id)  => new Link(src, id, Some(name)) }
		}
		else
			None
	}
	/** Generates a String identifying the provided Symbol that is stable across runs.
	* The Symbol must be a publicly accessible symbol, such as a method, class, or type member.*/
	private def stableID(sym: Symbol) =
	{
		val tpe = if(sym.isTerm) "term" else "type"
		val name = Compat.nameString(sym)
		val over = if(sym.isMethod) name + "(" + methodHash(sym) + ")" else name
		tpe + " " + over
	}
	// hack: probably not sufficient to distinguish all possible overloaded methods
	private def methodHash(sym: Symbol) = FileUtil.hash(sym.tpe.toString)
	private def relativeSource(file: File) = new File(getRelativeSourcePath(file.getAbsoluteFile))

	private object Compat
	{
		def nameString(s: Symbol): String = s.fullNameString
		/** After 2.8.0.Beta1, fullNameString was renamed fullName.*/
		private implicit def symCompat(sym: Symbol): SymCompat = new SymCompat(sym)
		private final class SymCompat(s: Symbol) {
			def fullNameString = s.fullName; def fullName = sourceCompatibilityOnly
		}
		private def sourceCompatibilityOnly = error("For source compatibility only: should not get here.")
	}
}

// for compatibility with 2.8
package forScope {
	class Sequence
	case class Comment(v: String, pos: util.RangePosition)
}
