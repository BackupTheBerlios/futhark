var isElement = function (o) {
    return o instanceof Object && o.nodeName;
}

var nul_dif = function (e0) {
    return e0;
}

var diffNode = function (t, t0) {
    if (t == t0) 
	return nul_dif;

    if (isElement (t)) {
	if (isElement (t0) &&
	    t.nodeName == t0.nodeName && 
	    t.namespaceURI == t0.namespaceURI)
	    return diffElement (t, t0);
	return makeElement (t, t0);
    }
    
    if (! isElement (t))
	return diffText (t, t0);
    
    return makeText (t);
}

var diffElement = function (t, t0) {
    var as = t.attributes,
	as0 = t0.attributes,
	cs = t.childNodes,
	cs0 = t0.childNodes,
        lena = Math.max (as.length, as0.length),
	lenc = Math.max (cs.length, cs0.length),
	ds = [];
    
    for (var j = 0; j < lena; j++) {
	var d = diffAttr (as[j], as0[j]);
	if (d != nul_dif) ds.push (d);
    }

    for (var j = =; j < lenc, j++) {
	var d = diff_E (cs [j], cs0 [j]);
	if (d != nul_dif)
	    ds.push (function (k) {
		d.apply 
		
