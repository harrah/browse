/* sxr -- Scala X-Ray
 * Copyright 2010  Olivier Michallat
 */

package sxr.vim

import java.io.{File, PrintWriter}
import java.net.URL
import sxr.{OutputWriter, OutputWriterContext, OutputInfo, Token}
import sxr.FileUtil.withWriter
import sxr.wrap.Wrappers

object VimWriter
{
	val PublicTags = "public-tags"
	val PrivateTags  = "private-tags"
	val RemotePublicTags = "remote-public-tags"
	val VimExtension = ".txt"
}
import VimWriter._
/** Outputs a set of files used by scala_sxr.vim.
 * These consist in: for each source file, a text file listing each token with its offset range,
 * its type, and the optional tag locating its declaration; and a global 'tags' file listing the
 * name, file and offset of each of these tags. */
class VimWriter(context: OutputWriterContext) extends OutputWriter {

	val outputDirectory = context.outputDirectory
	val info = new OutputInfo(outputDirectory, VimExtension)
	import info._

	private val excluded = Set.empty[String] ++ context.sourceFiles.map(_.getAbsolutePath)
	private val publicTagStore = new TagStore(new File(outputDirectory, PublicTags))
	private val publicTags = publicTagStore.read(excluded)

	private val privateTagStore = new TagStore(new File(outputDirectory, PrivateTags))
	private val privateTags = privateTagStore.read(excluded)

	def writeStart() {
		// Nothing to do
	}

	def writeUnit(sourceFile: File, relativeSourcePath: String, tokenList: List[Token]) {
		// The data file containing token info for this source file
		val outputFile = getOutputFile(relativeSourcePath)

		withWriter(outputFile) { output =>
			for (token <- tokenList) token.tpe match {
				case Some(t) => {
					// If this token references a declaration, find the corresponding Vim tag:
					// if the declaration is in another file, the name of the tag is the stable
					// ID, otherwise it is the internal token id.
					val targetTag = token.reference match {
						case Some(l) => l.stableID match {
							case Some(stable) => stable
							case _ => l.target toString
						}
						case None => ""
					}
					output.write(token.start + "\t" +
						(token.start + token.length - 1) + "\t" +
						t.name + "\t" +
						targetTag + "\n")

					// If this token defines a symbol, create the corresponding Vim tag(s)
					require(token.definitions.size <= 1, "Definitions were not collapsed for " + token)
					// In every case, create the tag with the internal ID. Even if the symbol is public, this tag will
					// still be used by the clients compiled in the same run.
					token.definitions.foreach((i: Int) => privateTags += Tag(i.toString, sourceFile.getAbsolutePath, token.start))
					// If the symbol is public, also create a tag for each stable ID. These tags will be used by clients
					// compiled in a different run where this unit would not be recompiled.
					token.stableIDs match {
						case i :: tail =>
							require(token.source.isDefined, "A token with stableIDs should have a source")
							token.stableIDs.foreach(stable => publicTags += Tag(stable, sourceFile.getAbsolutePath, token.start))
						case Nil => ()
					}
				}
				case _ => ()
			}
		}
	}
	
	def writeEnd() {
		publicTagStore.write(publicTags)
		privateTagStore.write(privateTags)

		writeRemotePublicTags(new File(outputDirectory, RemotePublicTags), context.externalLinkURLs)
	}

	/** Writes the file containing the path of the 'public-tags' file of each external sxr
	 *  location that resolves to a local directory. */
	private def writeRemotePublicTags(file: File, externalLinkURLs: List[URL]) {
		val remotePublicTags =
			externalLinkURLs.filter(_.getProtocol == "file").map(u => publicTags(urlToFile(u)))
		withWriter(file) { writer => writeAbsolutePaths(new PrintWriter(writer), remotePublicTags) }
	}
	private def writeAbsolutePaths(writer: PrintWriter, files: List[File]) {
		for (f <- files) writer.println(f.getAbsolutePath) 
	}

	/** Converts a "file://..." java.net.URL to a java.io.File.
	 * @see http://www2.java.net/blog/2007/04/25/how-convert-javaneturl-javaiofile */
	private def urlToFile(url: URL): File = {
		try {
			new File(url.toURI)
		} catch {
			case _ => new File(url.getPath)
		}
	}
	/** Returns the public tags file in the same directory as the given file. */
	private def publicTags(f: File) = new File(f.getParentFile, PublicTags)
}
