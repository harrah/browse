/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

import scala.tools.nsc.ast.parser.Tokens

private trait Styler
{
	def head: String
	def apply(token: Token): List[Annotation]
	def tail: String
}
private case class Annotation(open: String, close: String)

object Classes
{
	val Keyword = "keyword"
}
private class BasicStyler(title: String, baseStyle: String, baseJs: String, baseJQuery: String) extends Styler
{
	import Classes._
	def head =
		("""<?xml version="1.0" encoding="utf-8"?>
			|<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
			|<html xmlns="http://www.w3.org/1999/xhtml">
			|    <head>
			|        <meta http-equiv="Content-Type" content="text/html;charset=utf-8" ></meta>
			|        <title>""" + title + """</title>
			|        <script type="text/javascript" src=""" + '"' + baseJQuery + '"' + """></script>
			|        <script type="text/javascript" src=""" + '"' + baseJs + '"' + """></script>
			|        <link rel="stylesheet" type="text/css" href=""" + '"' + baseStyle + '"' + """ title="Style"></link>
			|    </head>
			|    <body>
			|        <pre>
			|""").stripMargin
	def tail =
		"""|
			|        </pre>
			|    </body>
		   |</html>
			|""".stripMargin
	
	def apply(token: Token) =
	{
		val styleClasses = classes(token.code)
		if(token.isPlain && styleClasses.isEmpty)
			Nil
		else
			annotateToken(token, styleClasses)
	}
	private def base(link: Link) = if(link.path == "") "" else link.path + ".html"
	private def constructHtmlLink(link: Link) = base(link) + "#" + link.target.id

	private def annotateToken(token: Token, styleClasses: List[String]) =
	{
		val tagName = if(token.isSimple) "span" else "a"
		val definitions = token.definitions
		require(definitions.size <= 1, "Definitions were not collapsed for " + token)
		val reference = token.reference.filter
			{ link =>
				val refID = link.target
				!definitions.contains(refID)
			}
		val definitionsList = definitions.toList
		val attributes = reference.map("href=\"" + constructHtmlLink(_) + "\"").toList :::
			token.tpe.map(t => "title=\"" + Escape(t.name) + "\"").toList :::
			definitionsList.headOption.map("id=\"" + _.id + "\"").toList :::
			( styleClasses match
			{
				case Nil => Nil
				case c => c.mkString("class=\"", ",", "\"") :: Nil
			})
		val extraIDs = if(definitionsList.isEmpty) Nil else definitionsList.tail.map(id => Annotation("<span id=\"" + id + "\">","</span>"))
		val main = Annotation("<" + tagName + " " + attributes.mkString(" ") + ">", "</" + tagName + ">")
		(main :: extraIDs).reverse // ensure that the a is always the most nested
		//addType(token, (main :: extraIDs).reverse)
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
		        case LPAREN | RPAREN | LBRACKET | RBRACKET | LBRACE | RBRACE => "delimiter" :: Nil
			case _ =>
				def isKeyword(code: Int) = code >= IF && code <= LAZY
				if(isKeyword(code))
					"keyword" :: Nil
				else
					Nil
		}
	}
}
