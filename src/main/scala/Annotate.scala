/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */

package sxr

import java.io.{File, Reader, Writer}
import scala.collection.jcl.TreeSet

/** The entry point for annotating an input file.*/
private object Annotate
{
	import FileUtil.{withReader, withWriter}
	/** Annotates an input source file with highlighting and type information provided by 'tokens' and applied by 'styler'.
	* The result is written to 'target'.*/
	def apply(source: File, target: File, tokens: TreeSet[Token], styler: Styler)
	{
		withReader(source) { input =>
			withWriter(target) { output =>
				new Annotate(input, output, tokens, styler).annotate()
			}
		}
	}
}
/** Annotates a source file.  This class is one-time use and should only be used through the Annotate module.
*
*  'input' is the raw source file.
* The annotated file will be written to 'output'
* The information associated with a file is defined by 'tokens'
* 'styler' generates the final annotations for a token 
*
* Note that the 'write' method in this class is specific to HTML and would
* need to be generalized for another format */
private class Annotate(input: Reader, output: Writer, tokens: TreeSet[Token], styler: Styler) extends NotNull
{
	/** Applies the annotations.*/
	def annotate()
	{
		output.write(styler.head)
		annotate(0)
		output.write(styler.tail)
	}
	/** Applies annotations.  index is the current position in the source file. */
	private def annotate(index: Int)
	{
		if(tokens.isEmpty) // no more tokens, copy the remaining characters over
			transfer(java.lang.Integer.MAX_VALUE)
		else
		{
			//look at the next token
			val token = tokens.firstKey
			tokens.remove(token)
			if(token.start < index)
			{
				println("Overlapping span detected at index " + index + ": " + token)
				annotate(index)
			}
			else
			{
				// copy over characters not to be annotated 
				transfer(token.start - index)
				// get the annotations for the token from the styler
				val styledList = styler(token)
				// write all opening tags
				for(styled <- styledList)
					output.write(styled.open)
				// copy the annotated content
				transfer(token.length)
				// close the tags
				for(styled <- styledList.reverse)
					output.write(styled.close)
				// continue
				annotate(token.start + token.length)
			}
			
		}
	}
	
	/** Transfers the given number of characters from the input to the output unless the input does not have enough
	* characters, in which case all remaining characters are transferred.*/
	private def transfer(chars: Int)
	{
		if(chars > 0)
		{
			val c = input.read()
			if(c >= 0)
			{
				write(c.asInstanceOf[Char])
				transfer(chars - 1)
			}
		}
	}
	/** Writes the given character to the output, escaping to HTML it if necessary.*/
	private def write(c: Char)
	{
		c match
		{
			case '>' => output.write("&gt;")
			case '&' => output.write("&amp;")
			case '<' => output.write("&lt;")
			case '"' => output.write("&quot;")
			case _ => output.write(c)
		}
	}
	
}
