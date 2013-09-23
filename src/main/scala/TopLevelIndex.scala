package sxr

	import scala.annotation.tailrec
	import java.nio.charset.Charset
	import java.net.URI

final class TopLevelLookup(val base: Option[URI], val relativeSource: String) {
	def resolve: String = base match {
		case None => relativeSource
		case Some(b) =>
			val rel = new URI(null, null, relativeSource, null)
			b.resolve(rel).toASCIIString
	}
}

sealed trait TopLevelIndex {
	def source(name: String): Option[TopLevelLookup]
}
final class MapIndex(val nameToSource: Map[String, String], val base: Option[URI]) extends TopLevelIndex
{
	def filterSources(keep: String => Boolean): MapIndex = new MapIndex(nameToSource.filter { case (name,src) => keep(src) }, base)
	def add(pairs: Iterable[(String,String)]): MapIndex = new MapIndex(nameToSource ++ pairs, base)
	def add(tli: MapIndex): MapIndex = new MapIndex(nameToSource ++ tli.nameToSource, base)
	def source(name: String): Option[TopLevelLookup] = nameToSource.get(name).map(s => new TopLevelLookup(base, s))
	def setBase(base: URI): MapIndex = new MapIndex(nameToSource, Some(base))
}
final class CompoundIndex(val is: Seq[TopLevelIndex]) extends TopLevelIndex
{
	def source(name: String): Option[TopLevelLookup] = headOption(is.iterator.flatMap(_.source(name)))
	private[this] def headOption[T](i: Iterator[T]) = if(i.hasNext) Some(i.next) else None
}

	import java.io._
	import FileUtil._

object TopLevelIndex
{
	val sep = " " + File.pathSeparator + " "

	def write(f: File, tli: MapIndex): Unit = withWriter(f) { writer =>
		asLines(tli).sorted.foreach(writeLine(writer))
	}
	def read(f: File): MapIndex =
		if(f.isFile) {
			withReader(f) { reader =>
				fromLines(readLines(reader, Nil))
			}
		} else
			new MapIndex(Map.empty, None)
	

	def compound(ts: Seq[TopLevelIndex]): TopLevelIndex = new CompoundIndex(ts)

	private[this] def asLines(tli: MapIndex): Seq[String] =
		tli.nameToSource.toSeq.map { case (name, src) => name + sep + src }

	private[this] def fromLines(ss: Seq[String]): MapIndex =
		new MapIndex(ss.map(lineToPair).toMap, None)

	private[this] def lineToPair(line: String): (String,String) =
	{
		val Array(name, src) = line.split(sep, 2)
		(name,src)
	}

	@tailrec
	private[this] def readLines(reader: BufferedReader, accum: List[String]): Seq[String] =
	{
		val line = reader.readLine
		if(line == null) accum.reverse else readLines(reader, line :: accum)
	}
	private[this] def writeLine(writer: BufferedWriter)(line: String)
	{
		writer.write(line)
		writer.newLine()
	}
}