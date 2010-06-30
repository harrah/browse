/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

import java.io.{FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter, InputStreamReader, OutputStreamWriter}
import java.net.URL

/** A collection of utilities for I/O*/
object FileUtil
{
	/** Managed resource operation.*/
	def withReader[T](source: File, sourceEncoding: String)(f: BufferedReader => T): T =
	{
		val input = new BufferedReader(new InputStreamReader(new FileInputStream(source), sourceEncoding))
		try { f(input) }
		finally { input.close() }
	}
	/** Managed resource operation.*/
	def withWriter[T](target: File)(f: BufferedWriter => T): T =
	{
		target.getParentFile.mkdirs()
		val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(target), DefaultEncoding))
		try { f(output) }
		finally { output.close() }
	}
	val DefaultEncoding = "UTF-8"
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
		if(source == null)
			error("Could not find resource " + resource)
		try { write(source, to) }
		finally { source.close() }
	}
	/** Writes the 'input' stream to the file 'to'.*/
	private def write(input: InputStream, to: File)
	{
		to.getParentFile.mkdirs()
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

	def readLines[T](file: File, encoding: String, value: T)(f: (T, String) => T): T =
		withReader(file, encoding) { reader => readLines(reader, value)(f) }
	private final def readLines[T](reader: BufferedReader, value: T)(f: (T, String) => T): T =
	{
		val line = reader.readLine()
		if(line eq null) value else readLines(reader, f(value, line))(f)
	}
	def readLines(file: File, encoding: String)(f: (String) => Unit): Unit =
		withReader(file, encoding) { reader => readLines(reader)(f) }
	private final def readLines(reader: BufferedReader)(f: (String) => Unit): Unit =
	{
		val line = reader.readLine()
		if(line ne null) { f(line); readLines(reader)(f) }
	}

	def download(url: URL, file: File) {
		download(url.openStream, file)
	}

	def downloadCompressed(url: URL, file: File) {
		download(new java.util.zip.GZIPInputStream(url.openStream), file)
	}

	def download(in: InputStream, file: File)
	{
		try { write(in, file) }
		finally { in.close() }
	}

	def hash(s: String): String = java.security.MessageDigest.getInstance("SHA").digest(s.getBytes).flatMap(hashDigits).mkString
	private def hashDigits(b: Byte) =
	{
		val i = toInt(b)
		Array( forDigit(i >> 4), forDigit(i & 0xF) )
	}
	import java.lang.{Character => C}
	private def forDigit(i: Int) = C.forDigit(i, C.MAX_RADIX)
	private def toInt(b: Byte): Int = if(b < 0) b + 256 else b
}
