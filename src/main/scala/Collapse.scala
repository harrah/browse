/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

private object Collapse
{
	def apply(tokens: Set[Token])
	{
		eliminateDuplicates(tokens)
		val c = new Collapse(tokens)
		c()
	}
	private def eliminateDuplicates(tokens: Set[Token])
	{
		val idOccurrences = new scala.collection.mutable.HashMap[StableID, Int] // map from definition ID to number of tokens with that ID
		for(token <- tokens; definition <- token.definitions.toSet[StableID])
			idOccurrences(definition) = idOccurrences.getOrElse(definition, 0) + 1
		// The set of all definition IDs used by more than one token.  These tokens generally indicate a problem: it is invalid to have tokens
		//   with the same ID.
		val duplicates = Set( idOccurrences.filter(_._2 > 1).map(_._1).toSeq : _*)
/*		for( (id, occur) <- idOccurrences if occur > 1)
			println("dup(" + occur + ") ID " + id.id + ": " + tokens.filter(_.definitions.contains(id)))*/
		tokens.foreach( _ --= duplicates)
	}
}
private class Collapse(tokens: Set[Token])
{
	private val collapsedIDMap = wrap.Wrappers.basicMap[StableID, StableID]
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
	private def remapTarget(oldID: StableID): StableID = collapsedIDMap.getOrElse(oldID, oldID)
}
