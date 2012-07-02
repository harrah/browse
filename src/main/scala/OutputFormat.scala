/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

import java.io.File

/** Enumerates the output formats handled by the plugin. */
object OutputFormat extends Enumeration {
	type OutputFormat = Value

	// The enumeration values
	val Html = Value("html")
	val Vim = Value("vim")
	def all: List[OutputFormat] = Html :: Vim :: Nil

	private[this] type Factory = OutputWriterContext => OutputWriter
	private[this] def factory(format: OutputFormat): Factory = format match {
		case Html => new HtmlWriter(_)
		case Vim => new vim.VimWriter(_)
	}

	/** Returns the writer corresponding to a value, configured with a context */
	def getWriter(value: OutputFormat, context: OutputWriterContext): OutputWriter =
		factory(value)(context)

	/** Returns the writer corresponding to a value, configured with a context */
	def getWriter(value: String, context: OutputWriterContext): OutputWriter = {
		val loader = this.getClass.getClassLoader
		val aClass = loader.loadClass(value)
		val constructor = aClass.getConstructor(classOf[OutputWriterContext])
		constructor.newInstance(context).asInstanceOf[OutputWriter]
	}
}
