

console.log('[Elpi-worker] ' + "Starting Elpi...");
importScripts('mlts.js');
console.log('[Elpi-worker] ' + "Elpi started");

var rep = { "type": "ready" };
postMessage(rep);

function sendLpl(code) {
    var rep = { "type": "lplcode", "code": code };
    postMessage(rep);
}

onmessage = function(event) {
    var code = event.data;
    console.log('[Elpi-worker] ' + "Compiling code");
    var lplcode = compile(code);
    
    sendLpl(lplcode[1]);

    if(lplcode[2] == 1) {
	console.log('[Elpi-worker] ' + "Querying run_all L.");
	var raw = run();
	console.log('[Elpi-worker] ' + "Returning answer.");
	var json = JSON.parse(raw);
	json.type = 'values';
	postMessage(json);
	
    }

    else {
	var res = JSON.parse("{}");
	res.type = 'error';
	postMessage(res);
    }
	    
}
