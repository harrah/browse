/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

import scala.tools.nsc.{plugins, Global, Phase}
import plugins.PluginComponent
import java.io.File

object BrowsePlugin
{
	val PluginName = "sxr"
	/** This is the name of the option that specifies the base directories against which sources should be relativized.*/
	val BaseDirectoryOptionName = "base-directory:"
}
/* The standard plugin setup.  The main implementation is in Browse.  The entry point is Browse.generateOutput */
class BrowsePlugin(val global: Global) extends Browse
{
	import global._
	import BrowsePlugin._
	
	val name = PluginName
	val description = "A plugin to produce a browsable representation of the input sources."
	val components = List[PluginComponent](Component)
	
	/** The directory to which the annotated sources will be written. */
	val outputDirectory = {
		val f = new File(settings.outdir.value)
		new File(f.getParent, f.getName + ".sxr").getAbsoluteFile
	}
	outputDirectory.mkdirs()

	/** The directory against which the input source paths will be relativized.*/
	private var baseDirectories: List[File] = Nil

	override def processOptions(options: List[String], error: String => Unit)
	{
		for(option <- options)
		{
			if(option.startsWith(BaseDirectoryOptionName))
				baseDirectories = parseBaseDirectories(option.substring(BaseDirectoryOptionName.length))
			else
				error("Option for source browser plugin not understood: " + option)
		}
	}
	def parseBaseDirectories(str: String): List[File] =
		str.split(File.pathSeparator).map(new File(_)).toList

	override val optionsHelp: Option[String] =
	{
		val prefix = "  -P:" + name + ":"
		Some(prefix + BaseDirectoryOptionName + "<paths>            Set the base source directories .\n")
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
		def run = generateOutput()
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