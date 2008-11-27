var zero = ['same'];

var diff = function (p, p0) {
    if (p == p0) return zero;
    if (p instanceof Array) {
	if (p0 instanceof Array) 
	    return diffArray (p, p0);
	return p;
    }
    if (p instanceof Object) {
	if (p0 instanceof Object)
	    return diffObject (p, p0);
	return p;
    }
    
    return p;
}

var diffArray = function (p, p0) {
    var l = Math.max (p.length, p0.length),
        r = [],
	isChanged = false;

    for (var j = 0; j < l; j++) {
	var dj = diff (p [j], p0 [j]); 
	if (! isChanged && dj != zero) isChanged = true;
	r [j] = dj;
    }
    return isChanged ? r : zero;
}

var diffObject = function (p, p0) {
    var r = {},
	isChanged = false;
    
    for (var j in p0) {
	var dj = diff (p [j], p0 [j]);
	r [j] = dj;
	if (! isChanged && dj != zero) isChanged = true;
	r [j] = dj;
    }
    for (var j in p) {
	var dj = diff (p [j], p0 [j]);
	r [j] = dj;
	if (! isChanged && dj != zero) isChanged = true;
	r [j] = dj;
    }
    
    return isChanged ? r : zero;
}

//--------------------------

var TEXT_NODE = 3;
var ELEMENT_NODE = 1;

var isTextNode = function (o) {
    return o.nodeType == TEXT_NODE;
}
var isElementNode = function (o) {
    return o.nodeType == ELEMENT_NODE;
}

var isElementPatch = function (pat) {
    return pat instanceof Object && pat.nodetype;
}

var patchNode = function (node, pat) {
    if (pat == zero) 
	return node;
   
    if (isElementPatch (pat))
	return isElementNode (node) ? patchElement (node, pat) : Dom.makeElementNode (pat);
    // else
    return isTextNode (node) ? patchText (node, pat) : Dom.makeTextNode (pat);
}

var patchTextNode = function (elem, pat) {
    elem.nodeValue = pat;
}

var patchElementNode = function (elem, pat) {
    if (pat.attributes != zero)
	patchAttributesNode (elem, pat.attributes);
    if (pat.childs != zero)
	patchChildsNode (elem, pat.childs);
    return elem;
}

var patchChildsNode = function (elem, pats) {
    var nodes = elem.childNodes;
    for (var j = 0; j < pats.length; j++) {
	var pat = pats [j],
	    node0 = nodes [j],
	    node1 = patchNode (node0, pat);
	if (node0 && node0 != node1) elem.replaceChild (node0, node1); // TODO COMPRESS
	if (! node0) elem.appendChild (node1);
    }    
}

var patchAttributesNode = function (elem, pats) {
    for (var j = 0; j < pats.length; j++) {
	var pat = pats [j];

	if (pat == zero)
	    continue;

	if (pat.name == 'style')
	    for (var k in pat) elem.style[k] = pat [k];
	
	elem.setAttributeNS (pat.namespace, pat.name, pat.value);
    }
}

var makeNodePatch = function (n, n0) {
    if (n == n0) return zero;
    if (n instanceof Object &&
	
    
    
	