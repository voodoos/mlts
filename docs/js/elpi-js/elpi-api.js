/**
 * @file Elpi-api
 * This file provide a small api to communicate
 * with elpi-worker to run lambda-prolog programs
 * in the browser
 *
 */

function generateUUID() { // Public Domain/MIT
  var d = new Date().getTime();
  if (typeof performance !== 'undefined' && typeof performance.now === 'function') {
    d += performance.now(); //use high-precision timer if available
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
    var r = (d + Math.random() * 16) % 16 | 0;
    d = Math.floor(d / 16);
    return (c === 'x' ? r : (r & 0x3 | 0x8)).toString(16);
  });
}

/**
 *  The main class, handling the lifecycle of
 * the Elpi Worker. 
 * 
 * */
class Elpi {
  /**
   * Creates a worker.
   * 
   * @callback loggerCB
   *    @param {string} lvl 
   *       The log level (Info, Warning or Error).
   *    @param {string} prefix
   *       The prefix, "who" sent the message.
   *    @param {string} text
   *       The text of the message
   *  The callback used when the Worker asks for logging
   * 
   * @callback answerCB
   *    @param {array(string)} args 
   *       The args of the answer
   *    @param {array(string)} assignements
   *       The assignements of the args
   *  The callback used when the Worker gives an answer
   * 
   */
  constructor(loggerCB, answerCB) {
    this.worker = null;

    this.logger = loggerCB;
    this.answer = answerCB;

    /* We cannot send directly callbacks to the worker
     * because functions are not clonable.
     * We store the callbacks for our promises
     * with a unique id and send the id to the worker. 
     * When the worker finishes its work it sends back 
     * the id and the callback is called.
     */
    this.resolves = [];
    this.rejects = [];


    var that = this;
    /* Message from the Elpi worker are 
    treated by the following function */
    this.onmessage = function (event) {
      var d = event.data;
      switch (d.type) {
        case "answer":
          answerCB(d.values);
          break;
        case "log":
          loggerCB(d.lvl, d.prefix, d.text);
          break;
        case "resolve":
          /* If it's a promise resolving, we use the id 
           * to call the correct callback */
          that.resolves[d.uuid](d.value);
          delete that.resolves[d.uuid];
          delete that.rejects[d.uuid];
          break;
        case "reject":
          /* If it's a promise rejection, we use the id 
          * to call the correct callback */
          that.rejects[d.uuid](new Error(d.value));
          delete that.resolves[d.uuid];
          delete that.rejects[d.uuid];
          break;
      }
    }

    /* The start property will store the promise
     * resolved at the end of the starting process */
    this.start = this.startElpi();
  }

  /**
   * Starts the Elpi Worker
   * It must be in the same folder.
   * 
   * Returns a promise which is stored 
   * in the start property.
   * 
   * @returns {Promise}
   */
  startElpi() {
    this.worker = new Worker("elpi-worker.js");
    this.worker.onmessage = this.onmessage;

    var that = this;

    return new Promise(function (resolve, reject) {
      // save callbacks for later
      that.resolves["start"] = resolve
      that.rejects["start"] = reject
    })
  }

  registerPromise(uuid, message) {
    var that = this;
    return new Promise(function (resolve, reject) {
      // save callbacks for later
      that.resolves[uuid] = resolve
      that.rejects[uuid] = reject

      that.worker.postMessage(message);
    })
  }

  /**
   * Sends some files for compilation to the Worker.
   * It returns a promise.
   * At the end of the execution the worker will resolve
   * the promise with the list of declared types. 
   * Or reject it with an error message.
   * 
   * @param {array({name: string, content: string})} files
   *   An array of files. Files are describded using two
   * strings: the name of the file and its content.
   *   All files in the array will be compiled and ready
   * to be queried (if no errors where found)
   * 
   * @param {bool=true} check
   *   Shoudl Elpi perform a static-check or not ?
   * 
   * @returns {Promise}
   */
  compile(files, check=true) {
    var uuid = generateUUID();
    var message = { type: "compile", files, check, uuid };

    return this.registerPromise(uuid, message)
  }


  /**
   * Sends the query to the worker. The worker will
   * then send successivley all the answers to that query.
   * It also returns a promise. 
   * At the end of the execution the worker will resolve
   * the promise with the array of all answers.
   * Or reject it with an error message.
   * 
   * @param {string} code
   *   The code of the query. It must end by a dot.
   *   For example "plus 2 4 Res."
   * 
   * @returns {Promise}
   */
  queryAll(code) {
    var uuid = generateUUID();
    var message = { type: "queryAll", code, uuid };

    return this.registerPromise(uuid, message)
  }

  /**
   * Stop and restart the Elpi Worker
   * 
   * Returns a promise which is stored 
   * in the start property.
   * 
   * @returns {Promise}
   */
  restart() {
    this.worker.terminate();
    /* We need to reject all non-resolved promises */

    var that = this
    Object.keys(this.rejects).forEach(function (r) {
      that.rejects[r](new Error("Elpi restarted"));
    });
    this.resolves = [];
    this.rejects = [];

    return (this.start = this.startElpi());
  }

}

export default Elpi;