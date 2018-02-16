

function mlog(message) {
    console.log('[Elpi-worker] ' + message);
}

mlog("Starting Elpi...");
importScripts('mlts.js');
mlog("Elpi started");

onmessage = function(event) {
    var code = event.data;
    mlog("Compiling code");
    var lplcode = compile(code);
    mlog("Querying run_all L.");
    var raw = run();
    mlog("Rerturning answer.");
    postMessage(JSON.parse(raw));
}
