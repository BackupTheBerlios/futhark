with (Yera) with (Actors) with (ActorsTest) {    
    var YeraCore = box (function () {
	
	var copy = function (o) {
	    var c = {};
	    for (var f in o) 
		c [f] = o [f];
	    return c;
	}
	var copyA = function (o) {
	    var c = [];
	    for (var f = 0; f < o.length; f++)
		c [f] = o [f];
	    return c;
	}

	var applyState = function (f, v) {
	    var r = f.value (box (function () {return v.value}));
	    var s = st (r);
	    
	    function setter (j) {
		return be(function (ev) {
		    var tf = f.then [j];
		    var tv = v.then [j];
		    return applyState (tf ? tf.behavior.apply(ev) : f, 
				       tv ? tv.behavior.apply(ev) : v);
		});
	    }
	    
	    for (var j in f.then)
		s.addSource (f.then [j].source, setter (j));

	    for (var j in v.then)
		if (! s.then[j])
		    s.addSource (v.then[j].source, setter (j));
	    
	    s.isEvent = f.isEvent || v.isEvent;
	    
	    return s;
	}
	
	var presState = function (v) {
	    var r = be (function (ev) {
		return st (v.value);
	    });
	    var s = st (r);
	    function setter (j) {
		var b1 = v.then [j].behavior;
		return be (function (ev) {
		    return presState (b1.apply (ev));
		});
	    }
	    for (var j in v.then)
		s.addSource (v.then [j].source, setter (j));
	    
	    s.isEvent = v.isEvent;
	    return s;
	}


	var futrState = function (v) {
	    var r = be (function (ev) {
		var sf = st (v.isEvent ? null : v.value);
		sf.then = v.then;
		sf.isEvent = v.isEvent;
		return sf;
	    });

	    var s = st (r);

	    function setter (j) {
		var b = v.then [j].behavior;
		return be(function (ev) {
		    return futrState (b.apply (ev));
		});
	    }
	    
	    for (var j in v.then)
		s.addSource (v.then [j].source, setter (j));
	    
	    s.isEvent = v.isEvent;
	    return s;
	}	 
	
	var untilState = function (i, e) {

	    var s = st (i.value);

	    function setter (j0) {
		return be (function (ev) {
		    if (e.value){
			var k = e.value.apply (null);
			return k.then [j0] ? k.then [j0].behavior.apply(ev):k;
		    }
		    return untilState (
			(i.then[j0] ? i.then[j0].behavior.apply(ev) : i),
			(e.then[j0] ? e.then[j0].behavior.apply(ev) : e));
		});
	    };
	    
	    for (var j in i.then)
		s.addSource (i.then[j].source, setter (j));
	    
	    for (var j in e.then)
		if (! s.then[j])
		    s.addSource (e.then[j].source, setter (j));

	    s.isEvent = i.isEvent;
	    
	    return s;
	}

	var untilIState = function (i, e) {

	    if (e.value) return e.value.apply (null);

	    var s = st (i.value);

	    function setter (j0) {
		return be (function (ev) {
		    return untilIState (
			(i.then[j0] ? i.then[j0].behavior.apply(ev) : i),
			(e.then[j0] ? e.then[j0].behavior.apply(ev) : e));
		});
	    };
	    
	    for (var j in i.then)
		s.addSource (i.then[j].source, setter (j));
	    
	    for (var j in e.then)
		if (! s.then[j])
		    s.addSource (e.then[j].source, setter (j));

	    s.isEvent = i.isEvent;
	    
	    return s;
	}
    
	var arrayState = function (os) {
	    var r = [];
	    for (var j = 0; j < os.length; j++) 
		r [j] = os [j].value;
	    
	    var s = st (r);

	    function setter (j) {
		var fs = tix [j];
		return be(function (ev) {
		    var os1 = copyA (os);
		    
		    for (var i = 0; i < fs.length; i++) {
			var f = fs [i];
			os1 [f] = os [f].then[j].behavior.apply (ev);
		    }
		    return arrayState (os1);
		});
	    }
	    var tix = [];
	    for (var f = 0; f < os.length; f++)
		for (var j in os[f].then) 
		    tix [j] ? tix [j] . push (f) : tix [j] = [f];
		
	    for (var j in tix) {
		var f = tix [j] [0];
		s.addSource (os [f].then[j].source, setter (j));
	    }
	    
	    for (var j = 0; j < os.length; j++) 
		if (os [j].isEvent) s.isEvent = true;
	    
	    return s;
	};
	
	var objectState = function (os) {
	    var r = {};
	    
	    for (var j in os) 
		r [j] = os [j].value;
	    
	    var s = st (r);

	    function setter (j) {
		var fs = tix [j];

		return be(function (ev) {
		    var os1 = copy (os);

		    for (var i = 0; i < fs.length; i++) {
			var f = fs [i];
			os1 [f] = os [f].then[j].behavior.apply (ev);
		    }
		    return objectState (os1);
		});
	    }
	    var tix = [];
	    for (var f in os) 
		for (var j in os[f].then) 
		    tix [j] ? tix [j] . push (f) : tix [j] = [f];
		
	    for (var j in tix) {
		var f = tix [j] [0];
		s.addSource (os [f].then[j].source, setter (j));
	    }

	    for (var j in os) 
		if (os [j].isEvent) s.isEvent = true;
	    
	    return s;
	};
	    
	var lift0 = box (function () {
	    return function (v) {
		return be (function (ev) {
		    return st (unbox (v))})}});

	var apply = box (function () {
	    return function (f) {
		return function (v) {
		    return be (function (ev){ 
			var fx = unbox (f).apply (ev),
			    vx = unbox (v).apply (ev);
			return applyState (fx, vx);
		    });
		}
	    }
	});
	
	var lift1 = box (function () {
	    return function (f) {
		return function (x) {
		    var b0 = box (function () {return unbox (lift0) (f)});
		    return unbox (apply) (b0) (x);
		}
	    }
	});
		
	var lift2 = box (function () {
	    return function (f) {
		return function (x) {
		    return function (y) {
			return unbox (apply) (box (function () {
			    return unbox (lift1) (f) (x)})) (y);
		    }
		}
	    }
	});
	
	var lift3 = box (function () {
	    return function (f) {
		return function (x) {
		    return function (y) {
			return function (z) {
			    var b0 = box (function () {return unbox (lift2) (f) (x) (y)});
			    return unbox (apply) (b0) (z);
			}
		    }
		}
	    }
	});

	var lift4 = box (function () {
	    return function (f) {
		return function (x) {
		    return function (y) {
			return function (z) {
			    return function (w) {
				var b0 = box (function () {return unbox (lift3) (f) (x) (y) (z)});
				return unbox (apply) (b0) (w);
			    }
			}
		    }
		}
	    }
	});

	var pres = box (function () {
	    return function (v) {
		return be (function (ev) {
		    return presState (unbox (v).apply (ev));
		});
	    }
	});

	var futr = box (function () {
	    return function (v) {
		return be (function (ev) {
		    return futrState (unbox (v).apply (ev));
		});
	    }
	});
	
	var until = box (function () {
	    return function (i) {
		return function (e) {
		    return be (function (ev) {
			return untilState (
			    unbox (i).apply (ev),
			    unbox (e).apply (ev));
		    });
		}
	    }
	});

	var untilI = box (function () {
	    return function (i) {
		return function (e) {
		    return be (function (ev) {
			return untilIState (
			    unbox (i).apply (ev),
			    unbox (e).apply (ev));
		    });
		}
	    }
	});
	   
	var liftArray = box (function () {
	    return function (os) {
		return be (function (ev) {
		    var s = [];
		    for (var j = 0; j < unbox(os).length; j++)
			s [j] = unbox (unbox (os) [j]).apply (ev);
		    return arrayState (s);
		});
	    }
	});
	var liftObject = box (function () {
	    return function (os) {
		return be (function (ev) {
		    var s = {};
		    for (var j in unbox (os)) 
			s [j] = unbox (unbox (os) [j]).apply (ev);
		    return objectState (s);
		});
	    }
	});
	

	var builtinLift1 = function (f) {
	    return box (function () {
		return unbox (lift1) (box (function () {
		    return function (x) {
			return f (unbox (x));
		    }}));
	    });
	};

	var builtinLift2 = function (f) {
	    return box (function () {
		return unbox (lift2) (box (function () {
		    return function (x) {
			return function (y) {
			    return f (unbox (x), unbox (y));
			}}}))})};

	var $em$un = box (function () {
	    return function (x) {
		return ! unbox(x);
	    }
	});

	var $em = box (function () {
	    return unbox (lift1) ($em$un);
	});
		       
	var negate$un = box (function () {
	    return function (x) {
		return  - unbox (x);
	    }
	});

	var negate = box (function () {
	    return unbox (lift1) (negate$un);
	});

	var length$un = box (function () {
	    return function (x) {
		return unbox (x) . length; 
	    }
	});
	var length = box (function () {
	    return unbox (lift1) (length$un);
	});

	var $un$pl$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) + unbox (y);
		}
	    }
	});

	var $pl = box (function () {
	    return unbox (lift2) ($un$pl$un);
	});

	var $un$mn$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) - unbox (y);
		}
	    }
	});

	var $mn = box (function () {
	    return unbox (lift2) ($un$mn$un);
	});

	
	var $un$st$un = box (function () {
	    return function (x){
		return function (y){
		    return unbox (x) * unbox (y);
		}
	    }
	});

	var $st = box (function () {
	    return unbox (lift2) ($un$st$un);
	});

	var $un$sl$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) / unbox (y);
		}
	    }
	});

	var $sl = box(function () {
	    return unbox (lift2) ($un$sl$un);
	});

	var $un$pr$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) % unbox (y);
		}
	    }
	});

	var $pr = box (function () {
	    return unbox (lift2) ($un$pr$un);
	});

	var $un$gt$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) > unbox (y);
		}
	    }
	});

	var $gt = box (function () {
	    return unbox (lift2) ($un$gt$un);
	});
	
	var $un$lt$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) < unbox (y);
		}
	    }
	});

	var $lt = box (function () {
	    return unbox (lift2) ($un$lt$un);
	});

	var $un$gt$eq$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) >= unbox (y);
		}
	    }
	});

	var $gt$eq = box (function () {
	    return unbox (lift2) ($un$gt$eq$un);
	});
	
	var $un$lt$eq$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) <= unbox (y);
		}
	    }
	});

	var $lt$eq = box (function () {
	    return unbox (lift2) ($un$lt$eq$un);
	});

	var $un$eq$eq$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) == unbox (y);
		}
	    }
	});

	var $eq$eq = box (function () {
	    return unbox (lift2) ($un$eq$eq$un);
	});

	var $un$vb$vb$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) || unbox (y);
		}
	    }
	});

	var $vb$vb = box (function () {
	    return unbox (lift2) ($un$vb$vb$un);
	});

	var $un$nd$nd$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) && unbox (y);
		}
	    }
	});

	var $nd$nd = box (function () {
	    return unbox (lift2) ($un$nd$nd$un);
	});
	
	var $unref$un = box (function () {
	    return function (x) {
		return function (y) {
		    return unbox (x) && unbox (x) [unbox (y)];
		}
	    }
	});
	
	var ref = box (function () {
	    return unbox (lift2) ($unref$un);
	});

	var __concat = function (x, y) {
	    return (x ? x : []).concat(y ? y : []);
	}

	var $unconcat$un = box (function () {
	    return function (x) {
		return function (y) {
		    return __concat (unbox(x), unbox(y));
		}
	    }
	});
	
	var concat = box (function () {
	    return unbox (lift2) ($unconcat$un);
	});
	
	var $un$ex$un = box (function () {
	    return function (x) {
		return function (y) {
		    return Math.pow(unbox (x), unbox (y));
		}
	    }
	});

	var $ex = box (function () {
	    return unbox (lift2) ($un$ex$un);
	});
	
	var $unsubstring$un = box (function () {
	    return function (x) {
		return function (y) {
		    return function (z) {
			return unbox (x).substring (unbox(y), unbox (z));
		    }
		}
	    }
	});
	
	var substring = box (function () {
	    return unbox (lift3) ($unsubstring$un);
	});


	var map$un = box (function () {
	    return function (f) {
		return function (a) {
		    f = unbox (f);
		    a = unbox (a);
		    var r = [];
		    for (var j = 0; j < a.length; j++)
			r [j] = f (a [j]);
		    return r;
		}
	    }
	    
	});

	var map = box (function () {
	    return function (f) {
		return function (x) {
		    return unbox (lift2) (map$un) (unbox (lift0) (f)) (x);
		}
	    }
	});

	var foldl$un = box (function () {
	    return function (f) {
		return function (i) {
		    return function (a) {
			f = unbox (f);
			i = unbox (i);
			a = unbox (a);
			for (var j = 0; j < a.length; j++)
			    i = f (i, a[j]);
			return i;
		    }
		}
	    }
	});

	var foldl = box (function () {
	    return function (f) {
		return unbox (lift3) (foldl$un) (unbox (lift0) (f));
	    }
	});

	var foldr$un = box (function () {
	    return function (f) {
		return function (i) {
		    return function (a) {
			f = unbox (f);
			i = unbox (i);
			a = unbox (a);
			for (var j = a.length - 1; j >= 0; j--)
			    i = f (i, a[j]);
			return i;
		    }
		}
	    }
	});

	var foldr = box (function () {
	    return function (f) {
		return unbox (lift3) (foldr$un) (unbox (lift0) (f));
	    }
	});

	var year$un = box (function () {
	    return function (t) {
		return (new Date (unbox(t))).getYear ();
	    }
	});

	var year = box (function () {
	    return unbox (lift1) (year$un);
	});

	var month$un = box (function () {
	    return function (t) {
		return (new Date (unbox(t))).getMonth ();
	    }
	});

	var month = box (function () {
	    return unbox (lift1) (month$un);
	});

	var monthDay$un = box (function () {
	    return function (t) {
		return (new Date (unbox(t))).getDate ();
	    }
	});

	var monthDay = box (function () {
	    return unbox (lift1) (monthDay$un);
	});

	var weekDay$un = box (function () {
	    return function (t) {
		return (new Date (unbox(t))).getDay ();
	    }
	});

	var weekDay = box (function () {
	    return unbox (lift1) (weekDay$un);
	});
	

	var hour$un = box (function () {
	    return function (t) {
		return (new Date (unbox(t))).getHours ();
	    }
	});

	var hour = box (function () {
	    return unbox (lift1) (hour$un);
	});

	var minute$un = box (function () {
	    return function (t) {
		return (new Date (unbox(t))).getMinutes ();
	    }
	});

	var minute = box (function () {
	    return unbox (lift1) (minute$un);
	});

	var second$un = box (function () {
	    return function (t) {
		return (new Date (unbox(t))).getSeconds ();
	    }
	});

	var second = box (function () {
	    return unbox (lift1) (second$un);
	});

	var millisecond$un = box (function () {
	    return function (t) {
		return (new Date (unbox(t))).getMilliseconds ();
	    }
	});

	var millisecond = box (function () {
	    return unbox (lift1) (millisecond$un);
	});
	
	return {
	    lift0 : lift0,
	    apply : apply,
	    liftObject : liftObject,
	    liftArray : liftArray,
	    lift1 : lift1,
	    lift2 : lift2,
	    lift3 : lift3,
	    lift4 : lift4,
	    pres : pres,
	    futr : futr,
	    until : until,
	    untilI : untilI,

	    $em$un: $em$un,
	    negate$un: negate$un,
	    length$un: length$un,
	    $un$pl$un: $un$pl$un,
	    $un$mn$un: $un$mn$un,
	    $un$st$un: $un$st$un,
	    $un$sl$un: $un$sl$un,
	    $unref$un: $unref$un,
	    $unconcat$un: $unconcat$un,
	    $un$ex$un: $un$ex$un,
	    $un$pr$un: $un$pr$un,
	    $un$gt$un: $un$gt$un,
	    $un$lt$un: $un$lt$un,
	    $un$gt$eq$un: $un$gt$eq$un,
	    $un$lt$eq$un: $un$lt$eq$un,
	    $un$eq$eq$un: $un$eq$eq$un,
	    $un$nd$nd$un: $un$nd$nd$un,
	    $un$vb$vb$un: $un$vb$vb$un,
	    $unand$un: $un$nd$nd$un,
	    $unor$un: $un$vb$vb$un,
	    $unsubstring$un: $unsubstring$un,

	    $em: $em,
	    negate: negate,
	    length: length,
	    $pl: $pl,
	    $mn: $mn,
	    $st: $st,
	    $sl: $sl,
	    ref: ref,
	    concat: concat,
	    $ex: $ex,
	    $pr: $pr,
	    $gt: $gt,
	    $lt: $lt,
	    $gt$eq: $gt$eq,
	    $lt$eq: $lt$eq,
	    $eq$eq: $eq$eq,
	    $nd$nd: $nd$nd,
	    $vb$vb: $vb$vb,
	    and : $nd$nd,
	    or : $vb$vb,
	    substring: substring,

	    map$un: map$un,
	    foldr$un: foldr$un,
	    foldl$un: foldl$un,

	    map: map,
	    foldr: foldr,
	    foldl: foldl,

	    year$un: year$un,
	    month$un: month$un,
	    monthDay$un: monthDay$un,
	    weekDay$un: weekDay$un,
	    hour$un: hour$un,
	    minute$un: minute$un,
	    second$un: second$un,
	    millisecond$un: millisecond$un,

	    year: year,
	    month: month,
	    monthDay: monthDay,
	    weekDay: weekDay,
	    hour: hour,
	    minute: minute,
	    second: second,
	    millisecond: millisecond
	};
    });
}
