/* sxr -- Scala X-Ray
 * Copyright 2009, 2010 Mark Harrah, Olivier Michallat
 */

package sxr

import scala.tools.nsc.{ast, plugins, symtab, util, Global}
import ast.parser.Tokens
import plugins.Plugin
import symtab.Flags
import reflect.internal.util.SourceFile
import annotation.tailrec

import java.io.{File, Reader, Writer}
import java.net.URL

import OutputFormat.{OutputFormat, getWriter}
import Browse._

class StableID(val id: String) extends AnyVal

object Browse
{
   /** The path to a link index, which allows linking between runs.  This applies for both incremental and remote linking.*/
	val LinkIndexRelativePath = "topLevel.index"
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
	/** The URLs of external sxr locations. */
	def externalLinkURLs: List[URL]
	/** Relativizes the path to the given Scala source file against the base directories. */
	def getRelativeSourcePath(source: File): String
	/** For a relativized source path, gets the full path. */
	def getFullSourcePath(relative: String): Option[File]
	/** The compiler.*/
	val global: Global

	import global._

	lazy val outputDirectory = new File(classDirectory.getParentFile, classDirectory.getName + ".sxr")
	outputDirectory.mkdirs

	private[this] val linkIndexFile = new File(outputDirectory, LinkIndexRelativePath)

	/** The entry method for invoking the configured writers to generate the output.*/
	def generateOutput(externalIndexes: List[TopLevelIndex])
	{
		val sourceFiles = currentRun.units.toList.flatMap(getSourceFile(_))

		val localIndex = getLocalIndex(sourceFiles)
		val combinedIndex = TopLevelIndex.compound(localIndex :: externalIndexes)

		if (sourceFiles.size > 0) {
			val context = new OutputWriterContext(sourceFiles, outputDirectory, settings.encoding.value, localIndex)
			val writers = outputFormats.map(getWriter(_, context))
			writers.foreach(_.writeStart())

			for(unit <- currentRun.units ; sourceFile <- getSourceFile(unit))
			{
				// generate the tokens
				val tokens = scan(unit)
				val traverser = new Traverse(tokens, unit.source, combinedIndex)
				traverser(unit.body)
				val tokenList = tokens.toList
				Collapse(tokenList.toSet)

				writers.foreach(_.writeUnit(sourceFile, getRelativeSourcePath(sourceFile), tokenList))
			}
			writers.foreach(_.writeEnd())
		}
	}
	private[this] def getLocalIndex(sourceFiles: List[File]): MapIndex =
	{
		val relativeSources = sourceFiles.map(getRelativeSourcePath(_)).toSet
		// approximation to determining if a top-level name is still present to avoid stale entries in the index
		def sourceExists(relativePath: String): Boolean = getFullSourcePath(relativePath).isDefined
		val localIndex = TopLevelIndex.read(linkIndexFile).filterSources(src => relativeSources(src) && sourceExists(src)).add(topLevelMappings)
		TopLevelIndex.write(linkIndexFile, localIndex)
		localIndex
	}

	private[this] def topLevelMappings: Seq[(String,String)] =
	{
		val newMappings = new collection.mutable.HashMap[String, String]
		for {
			unit <- currentRun.units
			sourceFile <- getSourceFile(unit)
			relativeSource = getRelativeSourcePath(sourceFile)
			name <- topLevelNames(unit.body)
		}
			newMappings += (name -> relativeSource)
		newMappings.toSeq
	}
	private def getSourceFile(unit: CompilationUnit): Option[File] = unit.source.file.file match {
		case null => None // code compiled from the repl has no source file
		case f: File => Some(f.getAbsoluteFile)
	}
	/** Tokenizes the given source.  The tokens are put into an ordered set by the start position of the token.
	* Symbols will be mapped back to these tokens by the offset of the symbol.*/
	private def scan(unit: CompilationUnit) =
	{
		val tokens = wrap.Wrappers.treeSet[Token]
		def addComment(start: Int, end: Int) { tokens += new Token(start, end - start + 1, Tokens.COMMENT) }

		class Scan extends syntaxAnalyzer.UnitScanner(unit)
		{
			override def deprecationWarning(off: Int, msg: String) {}
			override def error(off: Int, msg: String) {}
			override def incompleteInputError(off: Int, msg: String) {}

			override def foundComment(value: String, start: Int, end: Int) {
				addComment(start, end)
				super.foundComment(value, start, end)
		   }
			override def foundDocComment(value: String, start: Int, end: Int) {
				addComment(start, end)
				super.foundDocComment(value, start, end)
			}
			override def nextToken() {
				val offset0 = offset
				val code = token

				super.nextToken()

				if(includeToken(code)) {
					val length = (lastOffset - offset0) max 1
					tokens += new Token(offset0, length, code)
				}
			}
		}
		if(unit.isJava)
			new syntaxAnalyzer.JavaUnitParser(unit).parse() // TODO: Java source support
		else {
			val parser = new syntaxAnalyzer.UnitParser(unit) { override def newScanner = new Scan }
			parser.parse()
		}

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
		tokensAt(tokens, offset).headOption
	/** Gets the token for the given offset.*/
	private def tokensAt(tokens: wrap.SortedSetWrapper[Token], offset: Int): List[Token] =
	{
		// create artificial tokens to get a subset of the tokens starting at the given offset
		// then, take the first token in the range
		tokens.range(new Token(offset, 1, 0), new Token(offset+1, 1, 0)).toList
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
		
	private class Traverse(tokens: wrap.SortedSetWrapper[Token], source: SourceFile, index: TopLevelIndex) extends Traverser
	{
		// magic method #1
		override def traverse(tree: Tree)
		{
			def handleDefault()
			{
				process(tree, index)
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
		private def process(t: Tree, index: TopLevelIndex)
		{
			def catchToNone[T](f: => T): Option[T] = try Some(f) catch { case e: UnsupportedOperationException => None }
			for(tSource <- catchToNone(t.pos.source) if tSource == source; token <- tokenAt(tokens, t.pos.point))
			{
				def processDefaultSymbol() =
				{
					if(t.hasSymbol && !ignore(t.symbol))
						processSymbol(t, token, source.file.file, index)
				}
				def processSimple() { token.tpe = TypeAttribute(typeString(t.tpe), None) }
				def processTypeTree(tt: TypeTree) { if(!ignore(tt.symbol)) processSymbol(tt, token, source.file.file, index) }
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
// The associated token is no longer the case keyword, but the pattern, so this would overwrite the pattern's type.
//					case _: CaseDef => processSimple() // this will annotate the 'case' keyword with the type returned by that particular case statement
					case _: Throw => processSimple()
					case ta: TypeApply => processSimple() // this fills in type parameters for methods
					case Ident(_) => processDefaultSymbol()
					case Literal(value) => processSimple() // annotate literals
					case tt: TypeTree =>
						if(token.isPlain)
							processSymbol(tt, token, source.file.file, index)
					case _ => ()
				}
			}
		}
	}
		// magic method #3
	private def processSymbol(t: Tree, token: Token, sourceFile: File, index: TopLevelIndex)
	{
		val sym = t.symbol
		def addDefinition()
		{
			for(id <- stableID(sym)) {
				token.source = getRelativeSourcePath(sourceFile)
				token += id
			}
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
					token.tpe = TypeAttribute(asString, linkTo(sourceFile, sType.typeSymbol, index))
				}
			case ts: TypeSymbol =>
				val treeType = t.tpe
				val sType =
					if(treeType == NoType) ts.info
					else treeType
				//println("Type symbol " + sym.id + ": " + typeString(sType))
				if(sType != null)
					token.tpe = TypeAttribute(typeString(sType), linkTo(sourceFile, sType.typeSymbol, index))
			case _ => ()
		}
		if(sym != null && sym != NoSymbol)
		{
			if(t.isDef)
				addDefinition()
			else
			{
				linkTo(sourceFile, sym, index) match
				{
					case Some(x) => token.reference = x
					case None => ()//addDefinition()
				}
			}
		}
	}

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

	/** Generates a link usable in the file 'from' to the symbol 'sym', which might be in some other file. */
	private def linkTo(from: File, sym: Symbol, index: TopLevelIndex): Option[Link] =
	{
		def externalURL: Option[String] = for(name <- topLevelName(sym); result <- index.source(name)) yield
			result.base match {
				case None => makeLink(from, result.relativeSource)
				case Some(b) => result.resolve
			}

		if(sym == null || sym == NoSymbol || sym.owner == NoSymbol)
			None
		else
			stableID(sym) flatMap { id =>
				val source = sym.sourceFile
				val url: Option[String] =
					if(source != null)
						Some(makeLink(from, source.file))
					else
						externalURL
				url map { u => new Link(u, id) }
			}
	}
	@tailrec
	private[this] def topLevelName(s: Symbol): Option[String] =
		if(s == null || s == NoSymbol || s.isEmptyPackage) None
		else
		{
			val encl = s.owner
			if(encl.isPackageClass || encl.isEmptyPackage) Some(fullNameString(s)) else topLevelName(encl)
		}

	private def makeLink(from: File, to: File): String =
		if(to == from) "" else FileUtil.relativePath(relativeSource(from), relativeSource(to))
	private def makeLink(from: File, toRelative: String): String =
		FileUtil.relativePath(relativeSource(from), new File(toRelative))

	private[this] def lookup(sym: Symbol): Option[File] =
		Option(sym.associatedFile).flatMap(_.underlyingSource).flatMap(f => classpathEntry(f.file))

	private[this] def classpathEntry(f: File): Option[File] =
		classpathFiles find { entry => FileUtil.relativize(entry, f).isDefined }

	// couldn't find a direct method to get the Seq[File]
	private[this] lazy val classpathFiles: Seq[File] =
		util.ClassPath.split(new tools.util.PathResolver(settings).result.asClasspathString).map(s => new File(s).getAbsoluteFile)

	/** Generates a String identifying the provided Symbol that is stable across runs.
	* The Symbol must be a publicly accessible symbol, such as a method, class, or type member.*/
	private def stableID(sym: Symbol): Option[StableID] =
		Some(new StableID(fullNameString(sym)))

	private[this] def normalize(sym: Symbol): Symbol =
		if(sym.isModuleClass) {
			val mod = sym.companionModule
			if(mod == NoSymbol) sym else mod
		} else if(sym.isCaseApplyOrUnapply) {
			val cClass = sym.owner.companionClass
      if(cClass != NoSymbol)
        cClass
      else
        sym.owner
    } else if(sym.isPrimaryConstructor)
			sym.owner
		else if(sym.isStable && sym.isMethod) {
			val get = sym.getter(sym.enclClass)
			if(get == NoSymbol) sym else get
		}
		else
			sym

	// hack: probably not sufficient to distinguish all possible overloaded methods
	// the hash can be truncated quite a bit: aren't distinguishing between many options
	private def methodHash(sym: Symbol) = FileUtil.quarterHash(sym.tpe.toString)
	private def relativeSource(file: File) = new File(getRelativeSourcePath(file.getAbsoluteFile))

	/** Constructs a decoded fully qualified name for the given symbol. */
	private def fullNameString(sym: Symbol): String =
	{
		require(sym != NoSymbol)
		val s = normalize(sym)
		val owner = s.owner
		require(owner != NoSymbol)
		val root = owner.isRoot || owner.isEmptyPackageClass
		val sep = if(s.isTerm) { if(root) "" else "." } else ";"
		val name = sep + nameString(s)
		if(root)
			name
		else
			fullNameString(owner) + name
	}
	private[this] def nameString(sym: Symbol): String =
	{
		val params = if(isOverloadedMethod(sym)) "(" + methodHash(sym) + ")" else ""
		sym.nameString + params
	}
	private[this] def isOverloadedMethod(sym: Symbol): Boolean =
		sym.isMethod && sym.owner.info.member(sym.name).isOverloaded

	private[this] def topLevelNames(tree: Tree): Seq[String] =
	{
		val names = new collection.mutable.ListBuffer[String]
		val t = new TopLevelTraverser {
			def name(n: String) { names += n }
		}
		t(tree)
		names.toList
	}

	private abstract class TopLevelTraverser extends Traverser
	{
		def name(n: String): Unit
		override final def traverse(tree: Tree)
		{
			tree match
			{
				case (_: ClassDef | _ : ModuleDef) if isTopLevel(tree.symbol) => name(tree.symbol.fullName)
				case p: PackageDef =>
					if(!p.symbol.isEmptyPackage)
						name(p.symbol.fullName)
					super.traverse(tree)
				case _ =>
			}
		}
		def isTopLevel(sym: Symbol): Boolean =
			(sym ne null) && (sym != NoSymbol) && !sym.isImplClass && !sym.isNestedClass && sym.isStatic &&
			!sym.hasFlag(Flags.SYNTHETIC) && !sym.hasFlag(Flags.JAVA)
	}

}
