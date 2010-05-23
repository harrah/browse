/* sxr -- Scala X-Ray
 * Copyright 2010 Mark Harrah
 */

package sxr

import java.net.URL

/** A LinkMap is used to link across sxr runs.*/
trait LinkMap
{
	/** Adds a mapping.*/
	def update(src: String, stableID: String, id: Int): Unit
	/** Remove all mappings for the given sources.*/
	def clear(srcs: Iterable[String]): Unit
	/** Retrieve the mapped ID and the source providing that ID.*/
	def apply(stable: String): Option[(String, Int)]
	/** All mappings.*/
	def get: Iterable[(String, Iterable[(String, Int)])]
}
/** Combines multiple LinkMaps.
'update' and 'clear' are only applied to `write`.
`apply` and `get` are applied across `write` and `readOnly`.*/
class CompoundLinkMap(write: LinkMap, readOnly: List[LinkMap]) extends LinkMap
{
	def all = write :: readOnly
	def update(src: String, stableID: String, id: Int) = write.update(src, stableID, id)
	def clear(srcs: Iterable[String]) = write.clear(srcs)
	def apply(stable: String) = all.flatMap{ _(stable) }.firstOption
	def get = all.flatMap(_.get)
}
class BasicLinkMap(base: Option[URL]) extends LinkMap
{
	import scala.collection._
	import mutable.HashMap

	private val map = new HashMap[String, HashMap[String, Int]]

	def update(src: String, stableID: String, id: Int)
	{
		val srcMap = map.getOrElseUpdate(src, new HashMap[String, Int])
		srcMap(stableID) = id
	}
	def clear(srcs: Iterable[String]): Unit = map --= srcs
	def get: Iterable[(String, Iterable[(String, Int)])] = map
	def apply(stable: String): Option[(String, Int)] =
		map.flatMap{ case (src, m) => m.get(stable).map(x => (source(src), x)) }.toSeq.firstOption
	def source(src: String) =
		base match { case Some(url) => new URL(url, src) toExternalForm; case None => src }
}

import java.io.{BufferedReader, File, PrintWriter}
import FileUtil.{readLines, withWriter}
object LinkMapStore
{
	def read(file: File, base: Option[URL]): LinkMap = (new LinkMapStore(file)).read(base)
}

/** Read/write a LinkMap to/from a file.
* The format is a head followed by mappings,
* which are pairs of integer IDs used within a run and the String IDs that are used to link between runs.
*
*   [source-path]
*    integer-ID stable-string-ID
**/
class LinkMapStore(file: File)
{
	def write(map: LinkMap): Unit =
		withWriter(file) { writer => write(new PrintWriter(writer), map) }
	def read(base: Option[URL]): LinkMap =
	{
		val map = new BasicLinkMap(base)
		if(file.exists) fillMap(map)
		map
	}

	private def fillMap(map: LinkMap): Unit =
		readLines(file, encoding, "") { (src, line) =>
			parseLine(line, map, src)
		}

	private def write(writer: PrintWriter, map: LinkMap): Unit =
		for( (src, srcMap) <- map.get)
		{
			writer.println("[" + src + "]")
			for( (path, id) <- srcMap )
				writer.println(" " + id.toString + " " + path)
		}

	private def parseLine(s: String, map: LinkMap, currentSrc: String) =
		s.trim match
		{
			case Definition(id, path) => map(currentSrc, path) = id.toInt; currentSrc
			case SourceSection(src) => src
			case _ =>  println("Couldn't parse: " + s); currentSrc
		}
	private val SourceSection = """\[(.+)\]""".r
	private val Definition = """(\d+)\s+(.+)""".r
	private val encoding = FileUtil.DefaultEncoding
}