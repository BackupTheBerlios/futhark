var Actors = (function () {

    var Actor = function (b) {
	this.mailbox = [];
	this.pointer = 0;
	this.cont = b;
	this.running = false;
    }
    
    var ActorError = function (m) {
	if (m) this.message = m;
    }
    ActorError.prototype = new Error ("actor error");
    ActorError.prototype.name = "Actor Error";
    
    var EndOfMailbox = function (m) {
	if (m) this.message = m;
    }
    EndOfMailbox.prototype = new ActorError ("end of mailbox");
    EndOfMailbox.prototype.name = "End Of Mailbox Error";

    var TerminatedProcess = function (a) {
	if (a) this.actor = a;
    };
    TerminatedProcess.prototype = new ActorError ("this process is terminated");
    TerminatedProcess.prototype.name = "Terminated Process Error";

    var Suspension = function (ct) {
	this.cont = ct;
    }
    Suspension.prototype = new ActorError ("suspension");

    var runnings = [];

    var current_actor = 0;
    
    var self = function () {
	return current_actor;
    }

    var currentM = function () {
	if (this.mailbox.length > this.pointer) 
	    return this.mailbox [this.pointer];
	throw new EndOfMailbox ();
    }
    
    var consumeM = function () {
	if (this.mailbox.length > this.pointer) 
	    return this.mailbox.splice(this.pointer, 1)[0];
	throw new EndOfMailbox ();
    }
    
    var rewindM = function () {
	this.pointer = 0;
    }

    var nextM = function () {
	this.pointer ++;
    }
    
    var resumeA = function () {
	current_actor = this;
	try {
	    this.cont ();
	    this.cont = function () {throw new TerminatedProcess (this)};
	} catch (e) {
	    if (e instanceof Suspension) {
		this.cont = e.cont;
	    } else {
		throw e;
	    }
	}
    }
    
    var current = function () {
	return current_actor.currentM ();
    }
    
    var consume = function () {
	return current_actor.consumeM ();
    }
    
    var next = function () {
	return current_actor.nextM ();
    }
    
    var rewind = function () {
	return current_actor.rewindM ();
    }

    var suspend = function (v) {
	throw new Suspension (v);
    }

    var resume = function (a) {
	if (! a) a = current_actor;
	if (! a.running) {
	    a.running = true;
	    runnings.push (a);
	}
	resumeAll ();
    }

    Actor.prototype.currentM = currentM;
    Actor.prototype.consumeM = consumeM;
    Actor.prototype.rewindM = rewindM;
    Actor.prototype.nextM = nextM;
    Actor.prototype.resumeA = resumeA;
    
    var actor = function (b) {
	var a = new Actor (b);
	resume (a);
	return a;
    }
    
    var send_actor = function (a, m) {
	a.mailbox.push (m);
	resume (a);
    }
    
    function resumeAll () {
	if (! current_actor) { 
	    while (runnings.length) {
		var a = runnings.shift ();
		a.running = false;
		a.resumeA ();
	    }
	    current_actor = null;
	}
    }
    
    var isRunning = function () {
	return runnings.length;
    };

    return {
	Actor : Actor,
	ActorError : ActorError,
	TerminatedProcess : TerminatedProcess,
	Suspension : Suspension,
	EndOfMailbox : EndOfMailbox,
	actor : actor,
	send_actor : send_actor,
	self : self,
	current : current,
	consume : consume,
	next : next,
	rewind : rewind,
	suspend : suspend,
	resume : resume,
	isRunning: isRunning
    };
}) ();
