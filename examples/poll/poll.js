with (Yera) {
    
    var poller, shower, domUpdater, controller, adapter;

    var control = function () {
	recv(
	    hasType (Update, function (m){ 
		send (domUpdater, m.value.domelement);
		if (m.value.choice) send (poller, {choice: m.value.choice});
		control ();
	    }));
    }
    // adapt adapts poller calls to userevent
    var adapt = function () {
	recv (
	    otherwise (function (m) {
		usereventFire ({title: m.title});
		usereventFire ({question: m.question});
		for (var j in m.result) {
		    usereventFire ({choice: j});
	            usereventFire ({newResult: {name: j, votes: m.result [j]}});
		}
		usereventFire ({end_loading: true});
		adapt1 (m.title, m.question, m.result);
	    }));
    }
    
    var adapt1 = function (title, question, result) {
	recv (
	    otherwise (function (m) {
		result [m.change] = m.votes;
		usereventFire ({changeResult: {name: m.change, votes: m.votes}});
		adapt1(title, question, result);
		    }));
    }
    
    window.onload = function () {
	// ActorsRemote.init ();
	initGebo({
	    onConnectionEstablished: function (ev) {
		shower = react (Main.bindings.main);
		domUpdater = idUpdater ('main');
 		controller = actor (control);
		adapter = actor (adapt);
		
		send (shower, new Register (controller, 'connect'));
		
		jsonGet ("/registry/poll", function (data) {
		    poller = data;
		    send (poller, {register: adapter});
		});
	    }
	});
	
	window.onunload = function () {
	    send (poller, {"unregister": adapter});
	};
    };
};