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

	private[this] type Factory = (File, String) => OutputWriter
	private[this] def factory(format: OutputFormat): Factory = format match {
		case Html => new HtmlWriter(_, _)
		case Vim => new vim.VimWriter(_, _)
	}

	/** Returns the writer corresponding to a value, configured with a class directory and an encoding */
	def getWriter(value: OutputFormat, outputDirectory: File, encoding: String): OutputWriter =
		factory(value)(outputDirectory, encoding)
}
