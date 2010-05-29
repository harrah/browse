/* sxr -- Scala X-Ray
 * Copyright 2010  Olivier Michallat
 */

package sxr.vim

import java.io.{BufferedReader, File, PrintWriter}
import sxr.FileUtil.{withReader, withWriter, DefaultEncoding}
import sxr.wrap.{Wrappers, SortedSetWrapper}

final class Tag(val name: String, val file: String, val offset: Int) extends Comparable[Tag] with NotNull {
	/** Sort by name and then by file */
	override def compareTo(that: Tag) = this.name.compareTo(that.name) match {
		case 0 => this.file.compareTo(that.file)
		case nonzero => nonzero
	}
	override def toString = name + "\t" + file + "\t" + ":goto " + (offset + 1)

	/** Make compareTo consistent with equals */
	override def equals(other: Any): Boolean = other match {
		case that: Tag => this.name == that.name && this.file == that.file
		case _ => false
	}
	override def hashCode: Int = 41 * ( 41 + name.hashCode) + file.hashCode
}
object Tag {
	def apply(n: String, f: String, o: Int) = new Tag(n, f, o)
}

class TagStore(file: File) {
	def write(tags: SortedSetWrapper[Tag]) {
		withWriter(file) { writer =>
			write(new PrintWriter(writer), tags)
		}
	}
	def read() = {
		val tags = Wrappers.treeSet[Tag]
		if (file.exists) fillTags(tags)
		tags
	}
	private def fillTags(tags: SortedSetWrapper[Tag]) {
		withReader(file, DefaultEncoding) { reader =>
			parseTag(reader.readLine()).foreach(tags += _)
		}
	}
	import TagStore.{TagDef, Header}
	private def parseTag(s: String): Option[Tag] = s trim match {
		case TagDef(n, f, o) => Some(Tag(n, f, o.toInt - 1))
		case Header => None // silently ignored
		case _ => println("Error parsing tag: " + s) ; None
	}
	private def write(writer: PrintWriter, tags: SortedSetWrapper[Tag]) {
		writer.println("!_TAG_FILE_SORTED\t1\t")
		writer.println("!_TAG_FILE_ENCODING\t" + DefaultEncoding + "\t")
		for (tag <- tags)
			writer.println(tag.toString)
	}
}
object TagStore {
	private val Header = """!_TAG_FILE_.*""".r
	private val TagDef = """(.*)\t(.*)\t:goto (\d+)""".r
}
