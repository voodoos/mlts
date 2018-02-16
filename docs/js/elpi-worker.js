

console.log('[Elpi-worker] ' +"Starting Elpi...");
importScripts('mlts.js');
console.log('[Elpi-worker] ' +"Elpi started");

onmessage = function(event) {
    var code = event.data;
    console.log('[Elpi-worker] ' +"Compiling code");
    var lplcode = compile(code);
    console.log('[Elpi-worker] ' +"Querying run_all L.");
    var raw = run();
    console.log('[Elpi-worker] ' +"Rerturning answer.");
    postMessage(JSON.parse(raw));
}
