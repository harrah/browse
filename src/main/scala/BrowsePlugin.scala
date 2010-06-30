/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

import scala.tools.nsc.{plugins, Global, Phase}
import plugins.PluginComponent
import java.io.File
import java.net.URL

import OutputFormat.{OutputFormat, Html}

object BrowsePlugin
{
	val PluginName = "sxr"
	/** This is the name of the option that specifies the base directories against which sources should be relativized.*/
	val BaseDirectoryOptionName = "base-directory:"
	/** This is the name of the option that specifies the desired output formats.*/
	val OutputFormatsOptionName = "output-formats:"
	/** The separator in the list of output formats.*/
	val OutputFormatSeparator = '+'
	/** This is the name of the options that specifies a file containing one URL per line for each external sxr location to link to. */
	val ExternalLinksOptionName = "link-file:"
}
/* The standard plugin setup.  The main implementation is in Browse.  The entry point is Browse.generateOutput */
class BrowsePlugin(val global: Global) extends Browse
{
	import global._
	import BrowsePlugin._
	
	val name = PluginName
	val description = "A plugin to produce a browsable representation of the input sources."
	val components = List[PluginComponent](Component)
	
	/** The directory against which the input source paths will be relativized.*/
	private var baseDirectories: List[File] = Nil
	var externalLinkURLs: List[URL] = Nil

	lazy val classDirectory = new File(settings.outdir.value)

	/** The output formats to write */
	var outputFormats: List[OutputFormat] = List(Html)

	override def processOptions(options: List[String], error: String => Unit)
	{
		for(option <- options)
		{
			if(option.startsWith(BaseDirectoryOptionName))
				baseDirectories = parseBaseDirectories(option.substring(BaseDirectoryOptionName.length))
			else if(option.startsWith(OutputFormatsOptionName))
				outputFormats = parseOutputFormats(option.substring(OutputFormatsOptionName.length))
			else if(option.startsWith(ExternalLinksOptionName))
				externalLinkURLs = parseExternalLinks(option.substring(ExternalLinksOptionName.length))
			else
				error("Option for source browser plugin not understood: " + option)
		}
	}
	def parseBaseDirectories(str: String): List[File] =
		str.split(File.pathSeparator).map(new File(_)).toList

	def parseOutputFormats(str: String): List[OutputFormat] = {
		def valueOf(s: String): Option[OutputFormat] = OutputFormat.all.find(_.toString == s)
			.orElse { error("Invalid sxr output format: " + s) ; None }
		str.split(OutputFormatSeparator).flatMap(valueOf).toList
	}

	private def parseExternalLinks(links: String): List[URL] =
	{
		val f = new File(links)
		if(f.exists) readExternalLinks(f) else { error("Link file does not exist: " + f.getAbsolutePath); Nil }
	}
	private def readExternalLinks(f: File): List[URL] =
		FileUtil.readLines(f, FileUtil.DefaultEncoding, externalLinkURLs) { ( links, line) =>
			parseURL(line.trim).toList ::: links
		}
	private def parseURL(line: String): Option[URL] =
		if(line.isEmpty || line.startsWith("#")) None else Some(new URL(line))

	override val optionsHelp: Option[String] =
	{
		val prefix = "  -P:" + name + ":"
		val base = prefix + BaseDirectoryOptionName + "<paths>            Set the base source directories."
		val formats = prefix + OutputFormatsOptionName + "<formats>          '" + OutputFormatSeparator +
			"'-separated list of output formats to write (available: " + OutputFormat.all.mkString(",") + " - defaults to: " + Html + ")."
		val link = prefix + ExternalLinksOptionName + "<path>            Set the file containing sxr link.index URLs for external linking."

		Some( Seq(base, formats, link).mkString("", "\n", "\n") )
	}

	/* For source compatibility between 2.7.x and 2.8.x */
	private object runsBefore { def :: (s: String) = s }
	private abstract class CompatiblePluginComponent(afterPhase: String) extends PluginComponent
	{
		val runsAfter = afterPhase :: runsBefore
	}
	private object Component extends CompatiblePluginComponent("typer")
	{
		val global = BrowsePlugin.this.global
		val phaseName = BrowsePlugin.this.name
		def newPhase(prev: Phase) = new BrowsePhase(prev)
	}

	private class BrowsePhase(prev: Phase) extends Phase(prev)
	{
		def name = BrowsePlugin.this.name
		def run = generateOutput(externalLinkMaps)
	}

	private def externalLinkMaps: List[LinkMap] = externalLinkURLs.map(getLinkMap)
	private def getLinkMap(link: URL) =
	{
		val index = new URL(link, Browse.LinkIndexRelativePath)
		val cached = cachedLinkFile(index)
		if(!cached.exists) {
			try {
				FileUtil.downloadCompressed(new URL(link, Browse.CompressedLinkIndexRelativePath), cached)
			} catch {
				case e: java.io.IOException => FileUtil.download(index, cached)
			}
		}
		LinkMapStore.read(cached, Some(link))
	}
	private def cachedLinkFile(link: URL): File =
	{
		val cacheDirectory = new File( new File(settings.outdir.value).getAbsoluteFile.getParentFile, Browse.CacheRelativePath)
		new File(cacheDirectory, FileUtil.hash(link.toExternalForm))
	}

	/** Relativizes the path to the given Scala source file against the base directories. */
	def getRelativeSourcePath(source: File): String =
		baseDirectories.flatMap { base => FileUtil.relativize(base, source) } match
		{
			case Nil => source.getName
			case x :: Nil => x
			case xs => xs reduceLeft shortest
		}
	private[this] def shortest(a: String, b: String) = if(a.length < b.length) a else b
}
