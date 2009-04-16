function initializeLinked()
{
	foreachElementWithID("a", addHandlers)
	foreachLink(null, addHandlers)
}
function addHandlers(element)
{
	if(element && !element.onmouseout)
	{
		element.onmouseout = outHandler
		element.onmouseover = overHandler
	}
}

function overHandler(e)
{
	highlight(e, "#FFaaaa")
}
function outHandler(e)
{
	highlight(e, "transparent")
}
function highlight(e, color)
{
	var element = getTarget(e)
	while(element.id || element.href)
	{
		var definition = getDefinition(element)
		if(definition)
			highlightDefinition(definition, color)
		else
		{
			var referenced = getReference(element)
			if(referenced)
				highlightReferences(referenced, color)
		}
		element = element.parentNode
	}
}
function getReference(element)
{
	return element.id
}
function getDefinition(element)
{
	var href = element.href
	if(href)
	{
		var idIndex = href.lastIndexOf('#')
		if(idIndex)
			return href.slice(idIndex+1)
		else
			return null
	}
	else
		return null
}
function highlightDefinition(id, color)
{
	setBackground(document.getElementById(id), color)
}
function highlightReferences(id, color)
{
	foreachLink(id,
		function (linkElement) { setBackground(linkElement, color) }
	)
}
function foreachLink(id, f)
{
	var linkElement, i, href, links
	if(id)
		href = '#' + id
	else
		href = null
	links = document.links
	for(i = 0; linkElement = links[i]; i++)
	{
		if(!href || endsWith(linkElement.href, href))
			f(linkElement)
	}
}
//base.endsWith(check)
function endsWith(base, check)
{
	var checkLength, baseLength
	checkLength = check.length
	baseLength = base.length
	return checkLength <= baseLength && base.slice(baseLength - checkLength) === check
}
function foreachElementWithID(elementName, f)
{
	var element, i, elements
	elements = document.getElementsByTagName(elementName)
	for(i = 0; element = elements[i]; i++)
	{
		if(element.id)
			f(element)
	}
}
function setBackground(element, color)
{
	if(element)
		element.style.background = color
}
// function getTarget from http://www.quirksmode.org !
function getTarget(e)
{
	var targ;
	if (!e) var e = window.event;
	if (e.target) targ = e.target;
	else if (e.srcElement) targ = e.srcElement;
	if (targ.nodeType == 3) // defeat Safari bug
		targ = targ.parentNode;
	return targ;
}