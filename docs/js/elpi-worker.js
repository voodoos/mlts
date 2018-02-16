

function log(message) {
    console.log('[Elpi-worker] ' + message);
}

log("Starting Elpi...");
importScripts('mlts.js');
log("Elpi started");

onmessage = function(event) {
    var code = event.data;
    log("Compiling code");
    var lplcode = compile(code);
    log("Querying run_all L.");
    var raw = run();
    log("Rerturning answer.");
    postMessage(JSON.parse(raw));
}
