/* browse -- Scala Source Browser
 * Copyright 2009 Mark Harrah
 */

package browse

import java.io.{FileOutputStream, InputStream, OutputStream}
import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}

/** A collection of utilities for I/O*/
object FileUtil
{
	/** Managed resource operation.*/
	def withReader(source: File)(f: BufferedReader => Unit)
	{
		val input = new BufferedReader(new FileReader(source))
		try { f(input) }
		finally { input.close() }
	}
	/** Managed resource operation.*/
	def withWriter(target: File)(f: BufferedWriter => Unit)
	{
		target.getParentFile.mkdirs()
		import java.io.{BufferedWriter, FileWriter}
		val output = new BufferedWriter(new FileWriter(target))
		try { f(output) }
		finally { output.close() }
	}
	/** Get the number of common path components from the root.*/
	private def commonPrefix[S](a: Array[S], b: Array[S]): Int =
	{
		def common(count: Int): Int =
		{
			if(count >= a.length || count >= b.length || a(count) != b(count))
				count
			else
				common(count+1)
		}
		common(0)
	}
	/** Converts the given file to an array of path component strings. */
	private def toPathArray(file: File): Array[String] =
	{
		def toPathList(f: File, current: List[String]): List[String] =
		{
			if(f == null)
				current
			else
				toPathList(f.getParentFile, f.getName :: current)
		}
		toPathList(file.getCanonicalFile, Nil).toArray
	}
	/** Creates a relative path from 'fromFile' to 'toFile' (for use in an 'href' attribute).*/
	def relativePath(fromFile: File, toFile: File): String =
	{
		val fromPath = toPathArray(fromFile)
		val toPath = toPathArray(toFile)
		val commonLength = commonPrefix(fromPath, toPath)
		val relativeTo = toPath.drop(commonLength)
		val parentsToCommon = (fromPath.length - commonLength - 1)
		require(parentsToCommon >= 0)
		val up = "../" * parentsToCommon
		relativeTo.mkString(up, "/", "")
	}
	
	/** Copies the 'resource' to be found on the classpath to the file 'to'.*/
	def writeResource(resource: String, to: File)
	{
		val source = getClass.getResourceAsStream(resource)
		try { write(source, to) }
		finally { source.close() }
	}
	/** Writes the 'input' stream to the file 'to'.*/
	private def write(input: InputStream, to: File)
	{
		val out = new FileOutputStream(to)
		try { transfer(input, out) }
		finally { out.close() }
	}
	/** Copies all bytes from the 'input' stream to the 'output' strem. */
	private def transfer(input: InputStream, out: OutputStream)
	{
		val buffer = new Array[Byte](8192)
		def transfer()
		{
			val read = input.read(buffer)
			if(read >= 0)
			{
				out.write(buffer, 0, read)
				transfer()
			}
		}
		transfer()
	}
	/** Relativies the path of the given file against the given base file.*/
	def relativize(baseFile: File, file: File): Option[String] =
	{
		val pathString = file.getCanonicalPath
		baseFileString(baseFile) flatMap { baseString =>
			if(pathString.startsWith(baseString))
				Some(pathString.substring(baseString.length))
			else
				None
		}
	}
	// used by relativize
	private def baseFileString(baseFile: File): Option[String] =
	{
		if(baseFile.isDirectory)
		{
			val cp = baseFile.getCanonicalPath
			assert(cp.length > 0)
			if(cp.charAt(cp.length - 1) == File.separatorChar)
				Some(cp)
			else
				Some(cp + File.separatorChar)
		}
		else
			None
	}
}
