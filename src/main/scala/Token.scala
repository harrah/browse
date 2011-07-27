/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

/** Represents a link to a definition.  The path is the relative path to the file in the source
* directory, target is the symbol ID targeted. stableID is an identifier that will survive
* successive sxr runs (only used for public symbols).
* Specific writers need to construct the actual path from these informations (e.g in
* HtmlWriter, add '.html' to get the relative path of the target HTML file, and append the
* ID separated by a '#'. */
class Link(val path: String, val target: Int, val stableID: Option[String]) extends NotNull
{
	// This can still be useful for debugging, but must not be used directly by a writer.
	override def toString = path + "#" + target
	def retarget(newTarget: Int) = new Link(path, newTarget, stableID)
}
/** Represents a token at the lexer level with associated type information.
* 'start' is the offset of the token in the original source file.
* 'length' is the length of the token in the original source file
* 'code' is the class of the token (see Tokens in the compiler)*/
case class Token(start: Int, length: Int, code: Int) extends NotNull with Ordered[Token] with Comparable[Token]
{
	require(start >= 0)
	require(length > 0)
	/** Tokens are sorted by their start position, so that they may be searched by offset in
	* the extraction code and then processed in sequence in the annotation code. */
	def compare(other: Token) = start compare other.start
	
	private[this] var rawType: Option[TypeAttribute] = None
	private[this] var rawReference: Option[Link] = None
	private[this] var rawDefinitions: List[Int] = Nil
	private[this] var rawSource: Option[String] = None
	private[this] var rawStableIDs: List[String] = Nil
	/** Sets the type information for this token. */
	def tpe_=(t: TypeAttribute)
	{
		if(rawType.isEmpty)
			rawType = Some(t)
	}
	/** Sets the defining location for the symbol for this token. */
	def reference_=(l: Link)
	{
		if(rawReference.isEmpty)
			rawReference = Some(l)
	}
	/** Sets the relative source path for this token. */
	def source_=(s: String)
	{
		if(rawSource.isEmpty)
			rawSource = Some(s)
	}
	/** Adds an ID for this token.  An ID is used to mark this token as the source of a symbol. */
	def +=(id: Int) { rawDefinitions ::= id }
	/** Adds a stable ID for this token. A stable ID is used to resolve links across runs. */
	def +=(stableID: String) { rawStableIDs ::= stableID }
	/** Removes the IDs in the given set from this token's definitions and references.*/
	def --=(ids: Set[Int])
	{
		rawDefinitions = rawDefinitions.filter(d => !ids.contains(d))
		rawReference = rawReference.filter(r => !ids.contains(r.target))
	}
	/** Gets the type information. */
	def tpe = rawType
	/** Gets the link to the defining location for this token. */
	def reference = rawReference
	/** Gets the definition IDs. */
	def definitions = rawDefinitions
	/** Gets the stable definition IDs. */
	def stableIDs = rawStableIDs
	/** Gets the relative path of the source containing this token. */
	def source = rawSource
	/** True if this token has no reference to a definition and has no definitions itself. */
	def isSimple = reference.isEmpty && definitions.isEmpty
	/** True if this token has no reference to a definition, has no definitions itself, and
	* has no type information. */
	def isPlain = isSimple && tpe.isEmpty
	def collapseDefinitions(to: Int) = { rawDefinitions = to :: Nil }
	def remapReference(remap: Int => Int)
	{
		rawReference match
		{
			case Some(refLink) =>
				val newID = remap(refLink.target)
				if(newID != refLink.target)
					rawReference = Some(refLink.retarget(newID))
			case None => ()
		}
	}
}
/** Holds type information.  This class will probably change to accomodate tokeninzing types. */
case class TypeAttribute(name: String, definition: Option[Link]) extends NotNull
