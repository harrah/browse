/* browse -- Scala Source Browser
 * Copyright 2009 Mark Harrah
 */

package browse

import scala.tools.nsc.{ast, plugins, symtab, util, Global}
import ast.parser.Tokens
import plugins.Plugin
import symtab.Flags
import util.SourceFile
import scala.collection.jcl.{TreeMap, TreeSet}

import java.io.{File, Reader, Writer}

/** The actual work extracting symbols and types is done here. */
abstract class Browse extends Plugin
{
	/** The directory to which the annotated sources will be written. */
	def outputDirectory: File
	/** The directory against which the input source paths will be relativized.*/
	def baseDirectory: Option[File]
	/** The compiler.*/
	val global: Global
	
	import global._
	import Browse._

	/** The entry method for producing a set of html files and auxiliary javascript and CSS files that
	* annotate the source code for display in a web browser. */
	def generateOutput()
	{
		val cssFile = new File(outputDirectory, CSSRelativePath)
		val jsFile = new File(outputDirectory, JSRelativePath)
		writeDefaultCSS(cssFile)
		writeJS(jsFile)
		for(unit <- currentRun.units)
		{
			val sourceFile = unit.source.file.file
			val relativeSourcePath = getRelativeSourcePath(sourceFile)
			val outputFile = new File(outputDirectory, relativeSourcePath + ".html")
			val relativizedCSSPath = FileUtil.relativePath(outputFile, cssFile)
			val relativizedJSPath = FileUtil.relativePath(outputFile, jsFile)

			// generate the tokens
			val tokens = scan(unit)
			val traverser = new Traverse(tokens, unit.source)
			traverser(unit.body)

			val styler = new BasicStyler(tokens, relativeSourcePath, relativizedCSSPath, relativizedJSPath)
			Annotate(sourceFile, outputFile, tokens, styler)
		}
	}
	/** Tokenizes the given source.  The tokens are put into an ordered set by the start position of the token.
	* Symbols will be mapped back to these tokens by the offset of the symbol.*/
	private def scan(unit: CompilationUnit) =
	{
		val tokens = new TreeSet[Token]
		val scanner = new syntaxAnalyzer.UnitScanner(unit) { override def init {} }
		for( (offset, length, code) <- scanner.iterator)
		{
			if(includeToken(code))
				tokens += new Token(offset, length, code)
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
	private def tokenAt(tokens: TreeSet[Token], offset: Int): Option[Token] =
	{
		// create artificial tokens to get a subset of the tokens starting at the given offset
		val inRange = tokens.range(new Token(offset, 1, 0), new Token(offset+1, 1, 0))
		// then, take the first token in the range
		if(inRange.isEmpty)
			None
		else
			Some(inRange.firstKey)
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
		
	private class Traverse(tokens: TreeSet[Token], source: SourceFile) extends Traverser
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
			for(tSource <- t.pos.source if tSource == source; offset <- t.pos.offset; token <- tokenAt(tokens, offset))
			{
				def processDefaultSymbol() =
				{
					if(t.hasSymbol && !ignore(t.symbol))
						processSymbol(t, token, source.file.file)
				}
				def processSimple() { token.tpe = TypeAttribute(typeString(t.tpe), None) }
				def processTypeTree(tt: TypeTree) { if(!ignore(tt.symbol)) processSymbol(tt, token, source.file.file) }
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
					case Apply(fun, args) => //processDefaultSymbol()
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
							processSymbol(tt, token, source.file.file)
					case _ => ()
				}
			}
		}
	}
		// magic method #3
	private def processSymbol(t: Tree, token: Token, sourceFile: File)
	{
		val sym = t.symbol
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
				val asString =
					sType match
					{
						case mt: MethodType if ts.hasFlag(Flags.IMPLICIT)=> "implicit " + fullName(sym) + " : " + typeString(sType)
						case _ => typeString(sType)
					}
				//println("Term symbol " + sym.id + ": " + asString)
				token.tpe = TypeAttribute(asString, linkTo(sourceFile, sType.typeSymbol))
			case ts: TypeSymbol =>
				val treeType = t.tpe
				val sType =
					if(treeType == NoType) ts.info
					else treeType
				//println("Type symbol " + sym.id + ": " + typeString(sType))
				token.tpe = TypeAttribute(typeString(sType), linkTo(sourceFile, sType.typeSymbol))
			case _ => ()
		}
		if(t.isDef)
			token += sym.id
		else
		{
			linkTo(sourceFile, sym) match
			{
				case Some(x) => token.reference = x
				case None => token += sym.id
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
			case _ => t.toString
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
	private def linkTo(from: File, sym: Symbol): Option[Link] =
	{
		if(sym == NoSymbol)
			None
		else
		{
			val source = sym.sourceFile
			if(source == null)
				None
			else
			{
				val file = source.file
				val base = if(file == from) "" else FileUtil.relativePath(from, source.file) + ".html"
				Some(new Link(base, sym.id.toString))
			}
		}
	}
	/** Relativizes the path to the given Scala source file to the base directory. */
	private def getRelativeSourcePath(source: File): String =
	{
		baseDirectory match
		{
			case Some(base) =>
				FileUtil.relativize(base, source) match
				{
					case Some(relative) => relative
					case None => error("Source " + source + " not in base directory " + base); ""
				}
			case None => source.getName
		}
	}
}

object Browse
{
	/** The location to store the style sheet relative to the output directory.*/
	val CSSRelativePath = "style.css"
	/** The location to store the script relative to the output directory.*/
	val JSRelativePath = "linked.js"
	/** The path of the default style sheet resource.*/
	val DefaultCSS = "/default-style.css"
	/** The path of the default script resource.*/
	val LinkedJS = "/linked.js"
	
	/** Copies the default style sheet available as a resource on the classpath to the file 'to'.*/
	def writeDefaultCSS(to: File) { FileUtil.writeResource(DefaultCSS, to) }
	/** Copies the default script available as a resource on the classpath to the file 'to'.*/
	def writeJS(to: File) { FileUtil.writeResource(LinkedJS, to) }
}