with (Actors) with(ActorsTest){

    var Yera = function () {

	function Be (fn) {
	    this.apply = fn;
	}
	Be.prototype.name = "behavior";

	var mem_ids = 0;
	var mems = [];
	
	var be = function (fn) {
	    var id = mem_ids++;
	    return (new Be (function (ev) {
		var d = mems [id];
		return d ? d : mems [id] = fn (ev);
	    }));
	}
	
	var clearMemo = function () {
	    mems = [];
	};

	var St = function (v) {
	    this.value = v;
	    this.then = [];
	}
	St.prototype.name = "state";
	St.prototype.addSource = function (s, b) {
	    this.then [s.role] = {
		source: s,
		behavior: b
	    };
	};
	//     St.prototype.getPresent = function () {
	// 	var self = this;
	// 	return be (function (ev) {
	// 	    return st (self.value);
	// 	});
	//     }

	//     St.prototype.getFuture = function () {
	// 	var self = this;
	// 	return be (function (ev) {
	// 	    var sf = st (self.value);
	// 	    sf.then = self.then;
	// 	    return sf;
	// 	});
	//     }

	var st = function (v) {
	    return new St (v);
	};

	var src_ids = 0;

	var src = function (f) {
	    var r = actor (f);
	    r.role = src_ids++;
	    return r;
	};

	// events;
	function Register (a, r) {
	    this.from = a;
	    this.role = r;
	}
	Register.prototype.name = "register";

	function Unregister (a, r) {
	    this.from = a;
	    this.role = r;
	}
	Unregister.prototype.name = "unregister";

	function Update (r, v) {
	    this.role = r;
	    this.value = v;
	}
	Update.prototype.name = "update";
	
	var removeRole = function (ls, m) {
	    var r = [];
	    for (var j = 0; j < ls.length; j++) {
		var l = ls [j];
		if (l . from != m.from || l.role != m.role) r.push (l);
	    }
	    return r;
	}
	
	var reactimateChange = function (s0, s1, ls) {

	    for (var j in s1.then) 
		if (! s0.then [j])
		    send_actor (s1.then[j].source , new Register (self (), j))

	    for (var j in s0.then)
		if (! s1.then [j])
		    send_actor (s0.then[j].source , new Unregister (self (), j));;
	    
	    for (var j = 0; j < ls.length; j++) {
		var l = ls [j];
		send_actor (l.from, new Update (l.role, s1.value));
	    }
	    clearMemo ();
	    reactimate (s1, ls);
	}
	
	var reactimate = function (s0, ls) {
	    recv (
		cond (
		    hasType (Register, function (m) {
			send_actor (m.from, new Update (m.role, s0.value));
			reactimate (s0, ls.concat ([m]));
		    }),
		    hasType (Unregister, function (m) {
			var ls1 = removeRole (ls, m);
			if (ls1.length) reactimate (s0, ls1);
		    }),
		    hasType (Update, function (m) {
			if (s0.then[m.role]) {
			    var s1 = s0.then[m.role].behavior.apply (m.value);
			    reactimateChange (s0, s1, ls);
			} else {
			    // alert ("update unknow source");
			    reactimate (s0, ls);
			}
		    }),
		    otherwise (function (m) {
			alert ("otherwise reactimate");
			reactimate (s0, ls1);
		    })));
	}

	var reactimateInit = function (s) {
	    recv (hasType (Register, function (m) {
		for (var j in s.then) {
		    send_actor (s.then[j].source , new Register (self (), j));
		}
		send_actor (m.from, new Update (m.role, s.value));
		reactimate (s, [m]);
	    }));
	}
	
	var react = function (b) {
	    return src (function () {
		reactimateInit (b.apply (null));
	    });
	}

	var asText = function (v) {
	    if (v == null) return "null";
	    if (v == undefined) return "undefined";
	    return v;
	}
	
	var createElem,
	    createText,
            setAttr,
	    delAttr;

	if (document.createElementNS)
	    createElem = function (n, t) {
		return document.createElementNS (n, t);
	    }
	else if (document.createElement)
	    createElem = function (n, t) {
		if (n == "http://www.w3.org/1999/xhtml")
		    return document.createElement (t);
		else throw "javascript supports only html elements";
	    }
	else 
	    createElem = function (n, t) {
		throw "Unsupported browser";
	    }
	
	createText = function (t) {
	    return document.createTextNode (t);
	}

	if (document.documentElement.setAttributeNS)
	    setAttr = function (node, ns, nm, vl) {
		if (nm == "style" && (vl instanceof Object)) {
		    for (var j in vl)
			node.style [j] = vl [j];
		    return;
		}
		return node.setAttributeNS (ns, nm, vl);
	    };
	else if (document.documentElement.setAttribute)
	    setAttr = function (node, ns, nm, vl) {
		if (ns != "") throw "unsupported namespace";

		if (nm == "style" && vl instanceof Object){
		    for (var j in vl) 
			if (j == 'opacity') {
			    node.style ["filter"] = "alpha(opacity=" + (vl [j] * 100) + ")";
			} else node.style [j] = vl [j];
		    return;
		}
		if (nm == "style") 
		    return node.style.cssText = vl;

		if (nm == "class")
		    return node.setAttribute ("className", vl);

		return node.setAttribute (nm, vl);
	    };
	else 
	    setAttr = function (node, ns, nm, vl) {
		throw "unsupported browser";
	    }

	if (document.documentElement.removeAttributeNS)
	    delAttr = function (node, ns, nm, v0) { 
		if (nm == "style") {
		    for (var j in v0) 
			node.style [j] = null; 
		    return;
		}
		return node.removeAttributeNS (ns, nm);
	    };
	else if (document.documentElement.removeAttribute)
	    delAttr = function (node, ns, nm) {
		if (ns != "") throw "unsupported namespace";
		return node.removeAttribute (nm);
	    };
	else 
	    delAttr = function (node, ns, nm, vl) {
		throw "unsupported browser";
	    }
        
	var makeElement = function (v) {
	    if (! (v && v.nodetype))
		return createText (asText (v));
	    
	    var node = createElem (v.namespace, v.nodetype);
	    node.yera_value = v;

	    var as = v.attributes,
		cs = v.childs;
	    for (var j = 0; j < as.length; j++) {
		var a = as [j];
		setAttr (node, a.namespace, a.name, a.value);
	    }
	    
	    for (var j = 0; j < cs.length; j++)
		node.appendChild (toElement (cs [j]));

	    return node;
	}
	
	var toElement = function (v, c0) {
	    if (c0 && v && c0.yera_value == v) return c0;
	    if (c0 && v && c0.nodeName == v.nodetype && (c0.namespaceURI ? c0.namespaceURI == v.namespace : true))
		return restructElement (v, c0);
	    if (c0 && ((typeof v) == 'string') && c0.nodeType == 3)
		return restructText (v, c0);
	    return makeElement (v);
	}

	var restructText = function (v, c0) {
	    var t = asText (v);
	    c0.nodeValue == t ? null: c0.nodeValue = t;
	    return c0;
	}

	var restructElement = function (v, c0) {
	    var v0 = c0.yera_value;
	    var as = v.attributes,
		as0 = v0.attributes;
	    
	    for (var j = as.length; j < as0.length; j++) {
		var a0 = as0 [j];
		delAttr (c0, a0.namespace, a0.name, a0.value);
	    }
	    for (var j = 0; j < as.length; j++) {
		var a = as [j];
		setAttr (c0, a.namespace, a.name, a.value);
	    }
	    
	    var cs = v.childs,
		cs0 = c0.childNodes;
	    
	    for (var j = 0; j < cs.length ; j++) {
		var c = cs [j],
		    cx = cs0 [j],
		    c1 = toElement (c, cx);
		if (! cx) c0.appendChild (c1);
		else if (c1 != cx) c0.replaceChild (c1, cx);
	    }

	    for (var j = cs.length; j < cs0.length; j++)
		c0.removeChild (cs0 [j]);

	    c0.yera_value = v;

	    return c0;
	}
	 
	var idUpdater = function (nm) {
	    var child = document.getElementById (nm);
	    var father = child.parentNode;
	    var updater = function () {
		recv (
		    otherwise (function (value) {
			var c = toElement (value, child);
			if (c != child) father.replaceChild (c, child);
			child = c;
			updater ();
		    }));
	    }
	    return src (updater);
	}
	
	// convert messages of type Update to normal json values that are in this encapsulated
	var control = function (updater) {
	    recv (
		hasType (Update, function (m) {
		    send_actor (updater, m.value);
		    control(updater);
		}));
	}

	var connect = function (b, id) {
	    var be = react (b);
	    var ud = idUpdater (id);
	    var ct = src (function () {control (ud);});

	    send_actor (be, new Register (ct, "connect"));
	}

	var Interface = function (d) {
	    this.symbols = d;
	};
	Interface.prototype.name = "interface";

	var Struct = function (i, d) {
	    this.interface = i;
	    this.bindings = d;
	}
	Struct.prototype.name = "struct";
	
	var $dlunion = function (a) { 
	    return function (b) { 
		return new Interface (a.symbols.concat(b.symbols));
	    }
	}
	
	return {
	    Be : Be,
	    be : be,
	    St : St,
	    st : st,
	    src : src,
	    react: react,
	    Register: Register,
	    Unregister: Unregister,
	    Update: Update,
	    connect: connect,
	    idUpdater: idUpdater,
	    Struct: Struct,
	    Interface: Interface,
	    $dlunion: $dlunion
	};
    }();
}
