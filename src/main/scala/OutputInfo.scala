/* sxr -- Scala X-Ray
 * Copyright 2009, 2010 Mark Harrah, Olivier Michallat
 */
package sxr

import java.io.File

trait OutputInfo
{
	def outputDirectory: File
	/** Must include dot */
	def outputFileExtension: String
	def outputDirectorySuffix: String
	def getOutputFile(relativeSourcePath: String): File
}
abstract class DefaultOutput(classDirectory: File) extends OutputInfo
{
	val outputDirectory = {
		val dir = new File(classDirectory.getParentFile, classDirectory.getName + outputDirectorySuffix)
		dir.mkdirs
		dir
	}

	def getOutputFile(relativeSourcePath: String) =
		new File(outputDirectory, relativeSourcePath + outputFileExtension)
}