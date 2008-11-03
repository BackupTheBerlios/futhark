with (Actors) with (ActorsTest) with (ActorsMatch) with (ActorsRemote) with (Yera) {
    
    ActorsRemote.init ();
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
		userevent.fire ({title: m.title});
		userevent.fire ({question: m.question});
		for (var j in m.result) {
		    userevent.fire ({choice: j});
			    userevent.fire ({newResult: {name: j, votes: m.result [j]}});
		}
		userevent.fire ({end_loading: true});
		adapt1 (m.title, m.question, m.result);
	    }));
    }
    
    var adapt1 = function (title, question, result) {
	recv (
	    otherwise (function (m) {
		result [m.change] = m.votes;
		userevent.fire ({changeResult: {name: m.change, votes: m.votes}});
		adapt1(title, question, result);
		    }));
    }
    
    window.onload = function () {
	shower = reactB ('Main.main');
        domUpdater = idUpdater ('main');
 	controller = actor (control);
	adapter = actor (adapt);
	    
	send (shower, new Register (controller, 'connect'));
	
        poller = get ("/registry/poll");

        send (poller, {register: adapter});
    }
    
    window.onunload = function () {
	send (poller, {"unregister": adapter});
	}
}