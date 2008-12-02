with (Actors) with (ActorsMatch) with (ActorsTest) with (ActorsRemote) {

    var name, chatroom, main_actor;

    var main = function () {
	name = prompt ("What's your nickname?");
	if (! name) throw "without a nick you cannot enter in the chatroom";
	send (chatroom, {"enter": {"name": name, "address": self ()}});
	recv (
	    cond(
		match ("fail", function (bs) {
		    alert ("login failed, try another nickname.");
		    main ();
		}),
		match ({"welcome" : '?msg'}, function (bs) {
		    document.getElementById ("nick").appendChild(
			document.createTextNode(name + " says:"));
		    
		    document.getElementById ("body").appendChild (
			document.createTextNode (bs.msg));
		    
		    document.getElementById ("body").appendChild (
			document.createElement ("br"));
		    working ();
		})));
    };

    var working = function () {
	recv (
	    cond (
		match ({"message": {"body": '?body', "from": '?name'}}, function (bs) {
		    var body = bs.body;
		    var name = bs.name;
		    var bld = document.createElement ("b");

		    bld.appendChild (
			document.createTextNode (name + ":"));
		    bld.setAttribute("class","nick");
		    document.getElementById ("body").appendChild (bld);
		    
		    document.getElementById ("body").appendChild (
			document.createTextNode (body));
		    
		    document.getElementById ("body").appendChild (
			document.createElement ("br"));
		    working ();
		}),     
		match ({"newUser": {"name": '?name', "address" : '?address'}}, function (bs) {
		    document.getElementById ("body").appendChild (
			document.createTextNode (bs.name + " entered now."));
		    
		    document.getElementById ("body").appendChild (
			document.createElement ("br"));
		    working ();
		}),     
		match ({"exitUser": '?name'}, function (bs) {
		    if (bs.name) {
			document.getElementById ("body").appendChild (
			    document.createTextNode (bs.name + " exited now."));
			
			document.getElementById ("body").appendChild (
			    document.createElement ("br"));
		    }
		    working ();
		    }), 
		otherwise(function (kk) {
		    alert (kk);
		    working ();
		})));
    }

    var sendMessage = function () {
	var msg = document.getElementById ("message").value;
	if (msg.length) send (
	    chatroom, 
	    {"publish": 
	     {"from": name,
	      "body": msg}});
	document.getElementById("message").value = "";
    }
    
    window.onload = function () {
	ActorsRemote.init ();
	chatroom = get ("/registry/chatroom");
	main_actor = actor (main);
    };

    window.onunload = function () {
	send (chatroom, {'exit': {'name': name, 'address' : main_actor}});
    };
    
}// end of with (Actors) ...
