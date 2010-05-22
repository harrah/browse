/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

private object Collapse
{
	def apply(tokens: Iterable[Token])
	{
		eliminateDuplicates(tokens)
		val c = new Collapse(tokens)
		c()
	}
	private def eliminateDuplicates(tokens: Iterable[Token])
	{
		val idOccurrences = new scala.collection.mutable.HashMap[Int, Int] // map from definition ID to number of tokens with that ID
		for(token <- tokens; definition <- token.definitions)
			idOccurrences(definition) = idOccurrences.getOrElse(definition, 0) + 1
		// The set of all definition IDs used by more than one token.  These tokens are generally not significant and it is invalid to have tokens
		//   with the same ID.
		val duplicates = Set( idOccurrences.filter(_._2 > 1).map(_._1).toSeq : _*)
		tokens.foreach( _ --= duplicates)
	}
}
private class Collapse(tokens: Iterable[Token]) extends NotNull
{
	private val collapsedIDMap = wrap.Wrappers.basicMap[Int, Int]
	private def apply()
	{
		tokens.foreach(collapseIDs)
		tokens.foreach(_.remapReference(remapTarget))
	}
	private def collapseIDs(token: Token)
	{
		token.definitions match
		{
			case singleID :: b :: tail =>
				token.collapseDefinitions(singleID)
				(b :: tail).foreach(id => collapsedIDMap(id) = singleID)
			case _ => ()
		}
	}
	private def remapTarget(oldID: Int): Int = collapsedIDMap.getOrElse(oldID, oldID)
}