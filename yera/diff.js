var zero = ['same'];

var diff = function (p, p0) {
    if (p == p0) return zero;
    if (p instanceof Array) {
	if (p0 instanceof Array) 
	    return arrayDiff (p, p0);
	return p;
    }
    if (p instanceof Object) {
	if (p0 instanceof Object)
	    return diffObject (p, p0);
	return p;
    }
    
    return p;
}

var arrayDiff = function (p, p0) {
    var pl = p.length,
	pl0 = p0.length,
	l = pl > pl0 ? pl : pl0;
	r = [];

    for (var j = 0; j < l; j++)
	r [j] = diff (p [j], p0 [j]);
    return r;
}

var diffObject = function (p, p0) {
    var r = {};
    
    for (var j in p0)
	r [j] = diff (p [j], p0 [j]);

    for (var j in p)
	r [j] = diff (p [j], p0 [j]);
    
    return r;
}

var x = diff ({a: 1, b:2}, {c: 3, d: 4});