with (Actors) with (ActorsTest) {

    var ActorsMatch  = function () {

	var _ = ['any'];
	
	var unify_const = function (a, b, bs) {
	    if (a == b) return bs;
	    return false;
	}

	var unify_var = function (a,b,bs) {
	    var n = a.substring(1);
	    if (bs [n] != undefined) 
		return unify (b,bs [n], bs);
	    bs [n] = b;
	    return bs;
	}

	var unify_array = function (a, b, bs) {
	    if (! b instanceof Array) return false;
	    if (b.length != a.length) return false;
	    var r = bs;
	    for (var j = 0; j < a.length; j++)
		r = unify (a [j], b [j], r);
	    return r;
	}

	var unify_object = function (a, b, bs) {
	    if (! b instanceof Object) return false;
	    var r = bs;
	    for (var j in a)
		r = unify (a [j], b [j], r);

	    return r;
	}

	var unify = function (a, b, bs) {
	    if (a == _ || b == _)
		return bs;

	    if (a[0] == '?')
		return unify_var (a,b,bs);

	    if (b[0] == '?')
		return unify (b,a,bs);

	    if (typeof a == 'number' ||
		typeof a == 'string' ||
		typeof a == 'boolean')
		return unify_const(a,b,bs);

	    if (a instanceof Array)
		return unify_array (a,b,bs)

	    if (typeof a == 'object')
		return unify_object (a,b,bs);

	    return false;
	}

	match = function (t, ct) {
	    return when (
		function (c) {return unify (t, c, {})},
		function (c, r) {ct (r)})
	}

	return {
	    match: match,
	    _: _
	}
    }();
}

