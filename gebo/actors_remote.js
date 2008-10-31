with (Actors) { 

    var ActorsRemote = function () {
	var init = function () {
	    connection_id = http_uid ();
	    http_init_in ();
	    http_init_out ();
	}    

	var XHR;
	if (typeof XMLHttpRequest != 'undefined') {
	    XHR = function () {
		var req = new XMLHttpRequest();
		req.parseXml=function(){return req.responseXML};
		return req;
	    }
	}else if (typeof ActiveXObject != 'undefined') {
	    XHR = function () {
		var req=new ActiveXObject("Microsoft.XMLHTTP");
		req.parseXml=function(){
		    var doc = new ActiveXObject ("Microsoft.XMLDOM");
		    doc.loadXML(req.responseText);
		    return doc}
		return req;
	    }
	}else {
	    XHR = function () { throw "XMLHttpRequest unsupported" }
	}

	var host = "",
	    connection_id,
	    send_data,
	    mailbox = [],
	    actors = [],
	    actor_ids = 0;

	var stringToJson = function (s) {
	    return eval ("(" + s + ")");
	}

	var arrayToString = function (o) {
	    var s = "";
	    for (var j = 0; j < o.length; j++) 
		s+= ',' + jsonToString (o [j]);
	    if (s.length == 0) return "[]";
	    else return "[" + s.substring(1) + "]";
	}

	var objectToString = function (o) {
	    var s = "";
	    for (var j in o) 
		s+= ',' + jsonToString(j) + ':' + jsonToString (o [j]);
	    if (s.length == 0) return "{}";
	    else return "{" + s.substring(1) + "}";
	}

	var stringToString = function (o) {
	    var s = "";
	    for (var j = 0; j < o.length; j++) {
		var c = o.charAt(j);
		s+= c == '"' ? '\\"' : c;
	    }
	    return '"' + s  + '"';
	}

	var jsonToString = function (o) {
	    if (o instanceof Array) return arrayToString (o);
	    else if (typeof o == 'string') return stringToString (o);
	    else if (typeof o == 'number') return o;
	    else if (typeof 0 == 'function') throw "json conversion error : function";
	    else if (o instanceof Object) return objectToString (o);
	    else return stringToString (typeof o);
	    // else throw "json conversion error";
	}
	
	var http_uid = function () {
	    var con = XHR ();
	    con.open ("GET", host + "/gebo/uid", false);
	    con.send (null);
	    return stringToJson (con.responseText);
	}

	var http_init_in = function () {
	    var con = XHR();
	    con.open("GET", host + "/gebo/listen?" + connection_id, true);
	    con.onreadystatechange = function () {
		if (con.readyState == 4) {
		    con.onreadystatechange = null;
		    http_init_in ();
		    if (con.responseText) {
			var ms = stringToJson (con.responseText);
			for (var j = 0; j < ms.length; j++) {
			    var m = ms [j];
			    javascript_send (m.pid, m.message);
			}
		    }
		}
	    }
	    con.send (null);
	    return null;
	}

	var http_init_out = function () {
	    var con = XHR();
	    send_data = function () {
		send_data = function () {};
		var mb = mailbox;
		mailbox = [];
		con.open ("POST", host + "/gebo/notify?" + connection_id, true);
		con.onreadystatechange = function () {
		    if(con.readyState == 4) {
			con.onreadystatechange = null;
			http_init_out ();
		    }
		};
		con.send (jsonToString (mb));
	    }
	    if (mailbox.length > 0) send_data ();
	}

	var actor_send = function (a, msg) {
	    Actors.send (a, msg);
	}
	
	var remote_send = function (pid, msg) {
	    mailbox [mailbox.length] = {
		pid : pid, 
		message: msg};
	    send_data ();
	}
	
	var local_send = function (id, msg) {
	    Actors.send (idToActor (id["process-id"]), msg)
	}

	var scheme_send = function (id, msg) {
	    remote_send (id, actorToAddr (msg));
	}

	var javascript_send = function (id, msg) {
	    local_send (id, addrToActor (msg));
	}

	var idToActor = function (id) {
	    return actors [id];
	}

	var actorToId = function (a) {
	    var id = a.remote_actor_id;
	    if (! id) {
		id = ++actor_ids;
		a.remote_actor_id = id;
		actors [id] = a;
	    }
	    return id;
	}
	
	var actorToAddr = function (msg) {
	    if (msg instanceof Actor)
		return {"id-type": "javascript",
			"connection-id" : connection_id,
			"process-id" : actorToId(msg)
		       }
	    if (msg instanceof Array) {
		var r = [];
		for (var j = 0; j < msg.length; j++)
		    r [j] = actorToAddr (msg [j]);
		return r;
	    }
	    if (msg instanceof Object) {
		var r = {};
		for (var j in msg)
		    r [j] = actorToAddr (msg [j]);
		return r;
	    }
	    return msg;    
	}

	var addrToActor = function (msg) {
	    if (msg instanceof Object && msg["id-type"] == 'javascript')
		return idToActor (msg ["process-id"]);
	    if (msg instanceof Array) {
		var r = [];
		for (var j = 0; j < msg.length; j++)
		    r [j] = addrToActor (msg [j]);
		return r;
	    }
	    if (msg instanceof Object) {
		var r = [];
		for (var j in msg)
		    r [j] = addrToActor (msg [j]);
		return r;
	    }
	    return msg;    
	}
	
	var send = function (id, msg) {
	    if (id instanceof Actor) return actor_send (id, msg);
	    return scheme_send (id, msg);
	}

	// add code to handle result accordly to content type
	var get = function  (nm) {
	    var con = XHR ();
	    con.open ("GET", nm, false);
	    con.send (null);
	    return stringToJson (con.responseText);
	}

	var post = function (nm, what, ct) {
	    var con = XHR ();
	    con.open ("POST", nm, false);
	    con.send (what);
	    return stringToJson (con.responseText);
	}
	
	return {
	    init: init,
	    send: send,
	    get: get,
	    post: post
	};
    }();
}
