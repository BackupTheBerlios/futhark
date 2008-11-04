with (Actors) with (ActorsTest) {

    var ActorsMatch  = function () {
	
// 	var clone = function (o) {
// 	    var r = {};
// 	    for (var j in o) r [j] = o [j];
// 	    return r;
// 	}

	var _ = ['any'];
	
// 	var unify_const = function (a, b, bs) {
// 	    if (a == b) return bs;
// 	    return false;
// 	}

// 	var unify_var = function (a,b,bs) {
// 	    var n = a.substring(1);
// 	    if (bs [n] != undefined) 
// 		return unify (b,bs [n], bs);
// 	    var bs1 = clone (bs);
// 	    bs1 [n] = b;
// 	    return bs1;
// 	}

// 	var unify_array = function (a, b, bs) {
// 	    if (! (b instanceof Array)) return false;
// 	    if (b.length != a.length) return false;
// 	    var r = clone (bs);
// 	    for (var j = 0; j < a.length; j++)
// 		r = unify (a [j], b [j], r);
// 	    return r;
// 	}

// 	var unify_object = function (a, b, bs) {
// 	    if (! (b instanceof Object)) return false;
// 	    var r = clone (bs);
// 	    for (var j in a)
// 		r = unify (a [j], b [j], r);
// 	    return r;
// 	}

// 	var unify = function (a, b, bs) {
// 	    if (a == _ || b == _)
// 		return bs;

// 	    if (typeof a == 'string' && a.substring (0,1) == '?')
// 		return unify_var (a,b,bs);

// 	    if (typeof b == 'string' && b.substring (0,1) == '?')
// 		return unify (b,a,bs);

// 	    if (typeof a == 'number' ||
// 		typeof a == 'string' ||
// 		typeof a == 'boolean')
// 		return unify_const(a,b,bs);

// 	    if (a instanceof Array)
// 		return unify_array (a,b,bs)

// 	    if (a instanceof Object)
// 		return unify_object (a,b,bs);

// 	    return false;
// 	}

// 	var match = function (t, ct) {
// 	    return when (
// 		function (c) {return unify (t, c, {})},
// 		function (c, r) {ct (r)})
// 	}

	var isVar = function (s) {
	    return (typeof s == 'string') && (s.substring (0, 1) == '?');
	}

	var _matchVar = function (pat, val) {
	    var r = {};
	    r [pat.substring (1)] = val;
	    return r;
	}

	var _matchConst = function (pat, val) {
	    return pat == val ? {} : false;
	}

	var _matchArray = function (pat, val) {
	    if (! (val instanceof Array)) return false;
	    
	    var r = {};
	    for (var j = 0; j < pat.length; j++) {
		var r1 = _match (pat [j], val [j]);
		if (! r1) return false;
		for (var f in r1) r [f] = r1 [f];
	    }
	    return r;
	};

	var _matchObject = function (pat, val) {
	    if (! (val instanceof Object)) return false;
	    
	    var r = {};
	    for (var j in pat) {
		var r1 = _match (pat [j], val [j]);
		if (! r1) return false;
		for (var f in r1) r [f] = r1 [f];
	    }
	    return r;
	}
		
	var _match = function (pat, val) {
	    if (pat == _) return {};
	
	    if (isVar(pat))
		return _matchVar (pat, val);
	    
	    if (typeof pat == 'number' ||
		typeof pat == 'string' ||
		typeof pat == 'boolean')
		return _matchConst (pat, val);
	    
	    if (pat instanceof Array)
		return _matchArray (pat, val);
	    
	    if (pat instanceof Object)
		return _matchObject (pat, val);
	    
	    return false;
	}
	
	var match = function (t, ct) {
	    return when (
		function (c) {return _match (t, c, {})},
		function (c, r) {ct (r)})
	}

	return {
	    _match: _match,
	    match: match,
	    _: _
	}
    }();
}

