/* sxr -- Scala X-Ray
 * Copyright 2009, 2010 Mark Harrah, Olivier Michallat
 */

package sxr

import java.io.File

object HtmlWriter
{
	/** The location to store the index relative to the output directory.*/
	val IndexRelativePath = "index.html"
	/** The location to store the style sheet relative to the output directory.*/
	val StyleCssRelativePath = "style.css"

	/** The location to store the script relative to the output directory.*/
	val LinkedJsRelativePath = "linked.js"

	/** The location to store jQuery relative to the output directory.*/
	val JQueryJsRelativePath = "jquery-1.10.2.js"

	val JQueryScrollToJsRelativePath = "jquery-scrollto-1.4.9.js"

	val JQueryQTip2JsRelativePath = "jquery-qtip2-2.2.0.js"
	val JQueryQTip2CssRelativePath = "jquery-qtip2-css-2.2.0.css"

	val HtmlExtension = ".html"

	final val nl = System.getProperty("line.separator")
}

/** Outputs a set of html files and auxiliary javascript and CSS files that annotate the source
  * code for display in a web browser. */
class HtmlWriter(context: OutputWriterContext) extends OutputWriter {

	val outputDirectory = context.outputDirectory
	val encoding = context.encoding

	import HtmlWriter._
	val info = new OutputInfo(outputDirectory, HtmlExtension)

	import info._

	private var outputFiles = List[File]()

	val styleCssFile = new File(outputDirectory, StyleCssRelativePath)
	val linkedJsFile = new File(outputDirectory, LinkedJsRelativePath)
	val jQueryFile = new File(outputDirectory, JQueryJsRelativePath)
	val jQueryScrollToFile = new File(outputDirectory, JQueryScrollToJsRelativePath)
	val jQueryQTip2JsFile = new File(outputDirectory, JQueryQTip2JsRelativePath)
	val jQueryQTip2CssFile = new File(outputDirectory, JQueryQTip2CssRelativePath)

	def writeStart() {
		/** The location to store the style sheet relative to the output directory.*/
		FileUtil.writeResource(StyleCssRelativePath,styleCssFile)
		FileUtil.writeResource(LinkedJsRelativePath,linkedJsFile)
		FileUtil.writeResource(JQueryJsRelativePath,jQueryFile)
		FileUtil.writeResource(JQueryScrollToJsRelativePath,jQueryScrollToFile)
		FileUtil.writeResource(JQueryQTip2JsRelativePath,jQueryQTip2JsFile)
		FileUtil.writeResource(JQueryQTip2CssRelativePath,jQueryQTip2CssFile)
	}

	def writeUnit(sourceFile: File, relativeSourcePath: String, tokenList: List[Token]) {
		val outputFile = getOutputFile(relativeSourcePath)
		outputFiles ::= outputFile
		def relPath(f: File) = FileUtil.relativePath(outputFile, f)

		val styler = new BasicStyler(relativeSourcePath,
      relPath(styleCssFile),
      relPath(linkedJsFile),
      relPath(jQueryFile),
      relPath(jQueryScrollToFile),
      relPath(jQueryQTip2JsFile),
      relPath(jQueryQTip2CssFile))

		Annotate(sourceFile, encoding, outputFile, tokenList, styler)
	}

	def writeEnd(): Unit = writeIndex(new File(outputDirectory, IndexRelativePath))
	
	def writeIndex(to: File)
	{
		val relativizeAgainst = to.getParentFile
		val files = context.localIndex.nameToSource.values.toList.map(getOutputFile)
		val rawRelativePaths = files.flatMap(file => FileUtil.relativize(relativizeAgainst, file).toList)
		val sortedRelativePaths = wrap.Wrappers.treeSet[String]
		sortedRelativePaths ++= rawRelativePaths
		FileUtil.withWriter(to) { out =>
			out.write(
        s"""<html>
            |  <head>
            |    <meta http-equiv="Content-Type" content="text/html;charset=utf-8"/>
            |    <link rel="stylesheet" type="text/css" href="$StyleCssRelativePath" title="Style"/>
            |  </head>
            |  <body>
            |    <div id="index">
            |      <ul>
            |""".stripMargin)
			sortedRelativePaths.foreach(writeEntry(to, out))
      out.write(
        """      </ul>
          |    </div>
          |    <div id="content">
          |      <iframe name="target" id="target">
          |      </iframe>
          |    </div>
          |  </body>
          |</html>
          |""".stripMargin)
		}
	}
	import java.io.Writer
	private def writeEntry(index: File, out: Writer)(path: String)
	{
    val label =
      if(path.endsWith(".html"))
        path.substring(0, path.length - ".html".length)
      else
        path

    out.write( s"""        <li><a target="target" href="$path">$label</a></li>""")
    out.write(nl)
	}
}
