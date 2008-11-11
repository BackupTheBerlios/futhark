with (Yera) with (Actors) with (ActorsTest) with (unbox (YeraCore)) {
    var YeraDom = box (function () {
	
	var now = function () {return (new Date ()).getTime ()};
	
	var removeRole = function (ls, m) {
	    var r = [];
	    for (var j = 0; j < ls.length; j++) {
		var l = ls [j];
		if (l . from != m.from || l.role != m.role) r.push (l);
	    }
	    return r;
	}
	
	var __ticks__ = 1000 / 30;

	var timeUpdateInit = function () {
	    recv(
		hasType (Register, function (m) {
		    var me = self ();
		    var h = setInterval (function () {
			send_actor (me, new YeraEvent ("ciao"));
		    }, __ticks__);
		    var t = now ();
		    send_actor (m.from, new Update (m.role, t));
		    timeUpdate (t, d, h, [m]);
		}));
	}
		
	var timeUpdate = function (t, h, ls) {
	    recv ( 
		cond (
		    hasType (Register, function (m) {
			send_actor (m.from, new Update (m.role, t));
			timeUpdate (t, h, ls.concat ([m]));
		    }),
		    hasType (Unregister, function (m) {
			var ls1 = removeRole (ls, m);
			if (ls1.length > 0)
			    timeUpdate (t, h, ls1);
			clearInterval (h);
			timeUpdateInit ();
		    }),
		    hasType (YeraEvent, function (m) {
			var t1 = now ();
			for (var j = 0; j < ls.length; j++) {
			    var l = ls [j];
			    send_actor (l.from, new Update (l.role, t1));
			}
			timeUpdate (t1, h, ls);
		    })));
	}

	var timeSource = src (timeUpdateInit);

	var timeState = function (v) {
	    var r = st (v);
	    r.addSource(timeSource, be (timeState));
	    return r;
	}
	
	var time = box (function () {return be (function (ev) {
			return timeState (now ());
		    });
	    });

	var YeraEvent = function (ev) {
	    this.event = ev;
	    this.name = "YeraEvent";
	}
	
	var event_sources = {};

	var eventSource = function (x) {
	    var s = event_sources [x];
	    if (! s) s = event_sources [x] = src (function () {
		eventUpdateInit (x);
	    });
	    return s;
	}

	var registerEvent = window.addEventListener ? 
	    function (x, h) {
		window.addEventListener (x, h, false);
	    }: 
            function (x, h) {
		document.attachEvent ("on" + x, h);
	    };

	var unregisterEvent = window.removeEventListener ? 
	    function (x, h) {
		window.removeEventListener (x, h, false);
	    }:
            function (x, h) {
		document.detachEvent ("on" + x, h);
	    };
	    
	var eventUpdateInit = function (x) {
	    recv(
		hasType (Register, function (m) {
		    var me = self ();
		    var h = function (ev) {
			if (! ev) ev = window.event;
			if (ev.preventDefault) ev.preventDefault ();
			else try {ev.returnValue = false;
				 }catch (x) {};
			
			if (ev.srcElement) try {
			    ev.target = ev.srcElement;
			}catch (x) {}

			if (ev.toElement)
			    if (ev.type == 'mouseout') try {
				ev.relatedTarget = ev.toElement;
			    } catch (x) {} else try {ev.target = ev.toElement} catch (x) {};
			
			if (ev.fromElement)
			    if (ev.type == 'mouseout') try {
				ev.target = ev.fromElement;
			    } catch (x) {} else try {ev.relatedTarget = ev.fromElement} catch (x) {};

			send_actor (me, new YeraEvent (ev));
		    }
		    send_actor (m.from, new Update (m.role, null));
		    registerEvent (x, h);
		    eventUpdate (x, h, [m]);
		}));
	}
   
// 	var eventUpdateInit = function (x) {
// 	    recv(
// 		hasType (Register, function (m) {
// 		    var me = self ();
// 		    var h = function (ev) {
// 			ev.returnValue = false;
// 			ev.target = ev.srcElement;

// 			if (ev.toElement)
// 			    if (ev.type == 'mouseout') 
// 				ev.relatedTarget = ev.toElement;
// 			else ev.target = ev.toElement;
		    
// 			if (ev.fromElement)
// 			    if (ev.type == 'mouseout')
// 				ev.target = ev.fromElement;
// 			else ev.relatedTarget = ev.fromElement;

// 			send_actor (me, new YeraEvent (ev));
// 		    }
// 		    send_actor (m.from, new Update (m.role, null));
// 		    registerEvent (x, h);
// 		    eventUpdate (x, h, [m]);
// 		}));
// 	}
   
// 	var eventUpdateInit = function (x) {
// 	    recv(
// 		hasType (Register, function (m) {
// 		    var me = self ();
// 		    var h = function (ev) {
// 			if (! ev) ev = window.event;
// 			ev.preventDefault ();
// 			send_actor (me, new YeraEvent (ev));
// 		    }
// 		    send_actor (m.from, new Update (m.role, null));
// 		    registerEvent (x, h);
// 		    eventUpdate (x, h, [m]);
// 		}));
// 	}
	
	var eventUpdate = function (x, h, ls) {
	    recv(
		cond (
		    hasType (Register, function (m) {
			send_actor (m.from, new Update (m.role, null));
			eventUpdate (x, h, ls.concat ([m]));
		    }),
		    hasType (Unregister, function (m) {
			var ls1 = removeRole (ls, m);
 			if (ls1.length) 
			    eventUpdate (x, h, ls1);
			unregisterEvent (x, h);
			delete event_sources [x];
		    }),
		    hasType (YeraEvent, function (m) {
			for (var j = 0; j < ls.length; j++) {
			    var l = ls [j];
			    send_actor (l.from, new Update (l.role, m.event));
			    send_actor (l.from, new Update (l.role, null));
			}
			eventUpdate (x, h, ls);
		    })));
	}
	
	var event = box (function () {
	    return function (x) {
		return be (function (ev) {
		    var x0 = unbox (x).apply (ev);
		    return eventState (null, eventSource (x0.value), x0);
		});
	    }});
		       
	var eventState = function (v, s, x) {
	    var r = st (v);
	    r.addSource (s, be (function (e) {
		return eventState (e, s, x);
	    }));
	    
	    function setter (j) {
		return be (function (ev) {
		    var x1 = x.then [j].behavior.apply (ev);
		    return eventState (v, x1.value == x.value ? s : eventSource (x1.value), x);
		});
	    }
	    
	    for (var j in x.then)
		r.addSource (x.then [j].source, setter (j));
	    
	    r.isEvent = true;
	    return r;
	}
	
	var really_relative = function (t, x) {
	    if (! (t && t.yera_value)) return false;
	    if (! x) return false;
	    if (t.yera_value == x) return true;
	    if (t.parentNode) return really_relative (t.parentNode, x);
	    return false;
	}
	var $unrelative$un = box (function () {
	    return function (t) {
		return function (x) {
		    return really_relative (unbox (t), unbox (x));
		}
	    }
	});

	var relative = box (function () {
	    return unbox (lift2) ($unrelative$un);
	});

	return {
	    time: time,
	    event: event,
	    $unrelative$un: $unrelative$un,
	    relative: relative
	};
    });
}