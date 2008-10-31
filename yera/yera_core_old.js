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
				var b0 = box (function () {return unbox (lift2) (f) (x) (y) (z)});
				return unbox (apply) (lift3) (b0) (w);
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

	var $em = builtinLift1 (function (x) {return ! x});
	var negate = builtinLift1 (function (x) {return - x});
	var length = builtinLift1 (function (x) {return x.length});

	var $pl = builtinLift2 (function (x, y) {return x + y;});

	var $pl = box (function () {
	    return unbox (lift2) (box (function () {
		return function (_x) {
		    return function (_y) {
			return unbox (_x) + unbox (_y);
		    }}}));
	});
	var $mn = builtinLift2 (function (x, y) {return x - y;});
	var $st = builtinLift2 (function (x, y) {return x * y;});
	var $sl = builtinLift2 (function (x, y) {return x / y;});
	var $pr = builtinLift2 (function (x, y) {return x % y;});
	var $gt = builtinLift2 (function (x, y) {return x > y;});
	var $lt = builtinLift2 (function (x, y) {return x < y;});
	var $gt$eq = builtinLift2 (function (x, y) {return x >= y;});
	var $lt$eq = builtinLift2 (function (x, y) {return x <= y;});
	var $eq$eq = builtinLift2 (function (x, y) {return x == y;});
	var $vb$vb = builtinLift2 (function (x, y) {return x || y;});
	var $nd$nd = builtinLift2 (function (x, y) {return x && y;});

	var ref = builtinLift2 (function (x, y) {return x && x [y];});
	var concat = builtinLift2 (function (x, y) {return x .concat (y);});
	var $ex = builtinLift2 (function (x, y) {return Math.pow (x, y);});
	
	var year = builtinLift1 ( function (t) {
	    return (new Date (t)). getYear ();
	});

	var month = builtinLift1 (function (t) {
	    return (new Date (t)).getMonth ();
	});

	var monthDay = builtinLift1 (function (t) {
	    return (new Date(t)).getDate ();
	});

	var weekDay = builtinLift1 (function (t) {
	    return (new Date(t)).getDay ();
	});

	var hour = builtinLift1 ( function (t) {
	    return (new Date (t)).getHours ();
	});

	var minute = builtinLift1 (function (t) {
	    return (new Date (t)).getMinutes ();
	});

	var second = builtinLift1 (function (t) {
	    return (new Date (t)).getSeconds ();
	});
	
	var millisecond = builtinLift1 (function (t) {
	    return (new Date (t)).getMilliseconds ();
	});
	
	var gen_id = 0;
	var gensym = box (function () {
	    return function (p) {
		return be (function () {
		    return st ("yg:" + (++gen_id));
		})}});
	
	var _substring_ = box (function () {
	    return function (x) {
		return function (y) {
		    return function (z) {
			return unbox (x).substring (unbox(y), unbox (z));
		    }
		}
	    }
	});
	    	
	var substring = box (function () {
	    return unbox (lift3) (_substring_);
	});
	

	// NEW START

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

	var $unconcat$un = box (function () {
	    return function (x) {
		return function (y) {
		    return x ? x.concat (y ? y : "") : y;
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
	    year: year,
	    month: month,
	    monthDay: monthDay,
	    weekDay: weekDay,
	    hour: hour,
	    minute: minute,
	    second: second,
	    millisecond: millisecond,
	    gensym: gensym,
	    substring: substring
	};
    });
}
