/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */

package sbt

/** Escapes a raw string for use in HTML.*/
object Escape
{
	def apply(s: String) =
	{
		val out = new StringBuilder
		for(i <- 0 until s.length)
		{
			s.charAt(i) match
			{
				case '>' => out.append("&gt;")
				case '&' => out.append("&amp;")
				case '<' => out.append("&lt;")
				case '"' => out.append("&quot;")
				case c => out.append(c)
			}
		}
		out.toString
	}
}