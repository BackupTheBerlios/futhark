with (Actors) {    
    var ActorsTest = function () {
	
	var now = function () {
	    return (new Date ()).getTime();
	};

	var TestFailed = function (m) {
	    if (m) this.message = m;
	};
	TestFailed.prototype = new ActorError ("Test Failed");
	
	var recv = function (m ,t, a) {
	    try{ 
		m ();
	    } catch (e) {
		if (e instanceof TestFailed) {
		    next ();
		    recv (m, t, a);
		} else if (e instanceof EndOfMailbox) {
		    if (t != undefined) {
			suspendWithTimeout (m, t, a);
		    } else {
			suspendDefault (m);
		    }
		} else {
		    throw e;
		}
	    }
	};
	
	var suspendDefault = function (m) {
	    suspend (function () {
		return recv (m);
	    });
	};
	
	var suspendWithTimeout = function (m, t, a) {
	    var me = self ();
	    var t0 = now ();
	    var h = setTimeout (function () {
		me.cont = a;
		me.rewindM ();
		resume (me);
	    }, t);
	    suspend (function () {
		clearTimeout (h);
		var t1 = now ();
		var d = t1 - t0;
		recv (m, t - d, a);
	    });
	};
	
	var when = function (t, ct) {
	    return function () {
		var c = current ();
		var r = t (c);
		if (! r) throw new TestFailed ();
		consume ();
		rewind ();
		ct (c, r);
	    };
	};
	
	var cond = function () {return oneof (arguments);};
	
	var oneof = function (as) {
	    return function () {
		for (var j = 0; j < as.length; j++)
		    try {
			as [j] ();
		    } catch (e) {
			if (! (e instanceof TestFailed))
			    throw e;
		    }
		throw new TestFailed ();
	    };
	};
	
	var hasType = function (t, f) {
	    return when (
		function (c) {return c instanceof t;},
		function (c, r) { f (c); });
	};
	
	var otherwise = function (f) {
	    return when (
		function () {return true;},
		function (c, r) { f (c); });
	};
	
	return {
	    recv : recv,
	    when : when,
	    cond : cond,
	    
	    otherwise : otherwise,
	    hasType : hasType
	};
    }();
}