/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

import scala.collection.jcl.TreeSet
import scala.tools.nsc.ast.parser.Tokens

private trait Styler extends NotNull
{
	def head: String
	def apply(token: Token): List[Annotation]
	def tail: String
}
private case class Annotation(open: String, close: String) extends NotNull

object Classes
{
	val Keyword = "keyword"
}
private class BasicStyler(tokens: TreeSet[Token], title: String, baseStyle: String, baseJs: String) extends Styler
{
	import Classes._
	def head =
		("""<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
			|<html xmlns="http://www.w3.org/1999/xhtml">
			|    <head>
			|        <title>""" + title + """</title>
			|        <script type="text/javascript" src=""" + '"' + baseJs + '"' + """></script>
			|        <link rel="stylesheet" type="text/css" href=""" + '"' + baseStyle + '"' + """ title="Style">
			|    </head>
			|    <body onload="initializeLinked()">
			|        <pre>
			|""").stripMargin
	def tail =
		"""|
			|        </pre>
			|    </body>
		    |</html>""".stripMargin
	
	def apply(token: Token) =
	{
		val styleClasses = classes(token.code)
		if(token.isPlain && styleClasses.isEmpty)
			Nil
		else
			annotateToken(token, styleClasses)
	}
	private def annotateToken(token: Token, styleClasses: List[String]) =
	{
		val tagName = if(token.isSimple) "span" else "a"
		val definitions = Set(token.definitions : _*)
		val reference = token.reference.filter
			{ link =>
				if(link == BlockLink)
					false
				else
				{
					val refID = link.target.toInt
					!definitions.contains(refID)
				}
			}
		val definitionsList = definitions.toList
		val attributes = reference.map("href=\"" + _ + "\"").toList :::
			//token.tpe.map(t => "title=\"" + Escape(t.name) + "\"").toList :::
			definitionsList.firstOption.map("id=\"" + _ + "\"").toList :::
			( styleClasses match
			{
				case Nil => Nil
				case c => c.mkString("class=\"", ",", "\"") :: Nil
			})
		val extraIDs = if(definitionsList.isEmpty) Nil else definitionsList.tail.map(id => Annotation("<span id=\"" + id + "\">","</span>"))
		val main = Annotation("<" + tagName + " " + attributes.mkString(" ") + ">", "</" + tagName + ">")
		//(main :: extraIDs).reverse // ensure that the a is always the most nested
		addType(token, (main :: extraIDs).reverse)
	}
	private def addType(token: Token, baseAnnotations: List[Annotation]) =
	{
		val typeSpan = token.tpe.map(t => "<span class=\"type\">" + Escape(t.name) + "</span>").getOrElse("")
		if(typeSpan.isEmpty)
			baseAnnotations
		else
			Annotation("""<span class="typed">""" + typeSpan, "</span>") :: baseAnnotations
	}
	private def classes(code: Int) =
	{
		import Tokens._
		code match
		{
			case CHARLIT => "char" :: Nil
			case INTLIT => "int" :: Nil
			case LONGLIT => "long" :: Nil
			case FLOATLIT => "float" :: Nil
			case DOUBLELIT => "double" :: Nil
			case STRINGLIT => "string" :: Nil
			case SYMBOLLIT => "symbol" :: Nil
			case COMMENT => "comment" :: Nil
			case _ =>
				if(isKeyword(code))
					"keyword" :: Nil
				else
					Nil
		}
	}
}