object XML
{
	// xml not highlighted because the XML parser doesn't produce tokens
	val xx = <script language="javascript"><xml:unparsed>
...
elements[i].style.display = 'none';
...
    </xml:unparsed></script>


	def embed(q: Q) =
		<atag anAttr={3.toString}>
			<nested>
				{ q m 55 }
			</nested>
		</atag>
}
