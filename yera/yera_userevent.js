with (Yera) with (Actors) with (ActorsTest) {

    var userevent = (function () {
	var listeners = [];
	var addListener = function (f) {
	    listeners.push (f);
	}
	
	var removeListener = function (f) {
	    for (var j = 0; j < listeners.length; j++)
		if (listeners [j] == f) return listeners.splice (j, 1);
	};
	
	var fire = function (m) {
	    for (var j = 0; j < listeners.length; j++)
		listeners [j] (m);
	}
	

	return {
	    addListener: addListener,
	    removeListener: removeListener,
	    fire: fire
	};
    }) ();
	

    var YeraUserevent = box (function () {
	
	var removeRole = function (ls, m) {
	    var r = [];
	    for (var j = 0; j < ls.length; j++) {
		var l = ls [j];
		if (l . from != m.from || l.role != m.role) r.push (l);
	    }
	    return r;
	}
	
	var YeraUserevent = function (ev) {
	    this.event = ev;
	    this.name = "YeraUserevent";
	}

	var usereventUpdate = function (h, ls) {
	    recv(
		cond (
		    hasType (Register, function (m) {
			send_actor (m.from, new Update (m.role, null));
			usereventUpdate (h, ls.concat ([m]));
		    }),
		    hasType (Unregister, function (m) {
			usereventUpdate (h, removeRole (ls, m));
		    }),
		    hasType (YeraUserevent, function (m) {
			for (var j = 0; j < ls.length; j++) {
			    var l = ls [j];
			    send_actor (l.from, new Update (l.role, m.event));
			    send_actor (l.from, new Update (l.role, null));
			}
			usereventUpdate (h, ls);
		    }),
		    otherwise (function (m) {
			usereventUpdate (h, ls);
		    })));
	}
	
	var usereventSource = src (function () {
	    var me = self ();
	    var h = function (m) {
		send_actor (me, new YeraUserevent (m));
	    }
	    userevent.addListener (h);
	    usereventUpdate (h, []);
	});
		

	var usereventState = function (v0) {
	    var r = st (v0);
	    r.addSource (usereventSource, be (function (ev) {
		return usereventState (ev);
	    }));
	    r.isEvent = true;
	    return r;
	};

	var uevent = box (function () {
	    return be (function (ev) {
		return usereventState (null);
	    })
	});
	
	return {
	    uevent: uevent
	};
    });
}
