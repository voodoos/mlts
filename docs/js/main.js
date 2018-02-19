
// Initializing editor
var editor = ace.edit("editor");
editor.setTheme("ace/theme/ambiance");
editor.session.setMode("ace/mode/ocaml");

editor.commands.addCommand({
    name: 'build',
    bindKey: {win: 'Ctrl-B',  mac: 'Command-B'},
    exec: function(editor) {
        if(!($('#run_btn').is('[disabled]'))) run();
    },
    readOnly: true // false if this command should not apply in readOnly mode
});


editor.commands.addCommand({
    name: 'save',
    bindKey: {win: 'Ctrl-S',  mac: 'Command-S'},
    exec: function(editor) {
        save();
    },
    readOnly: true // false if this command should not apply in readOnly mode
});

// Initializing tooltips:
$(function () {
  $('[data-toggle="tooltip"]').tooltip()
})

// Loading readme :
$(document).ready(function(){ 
  $.get("readme.html", function(data) {
    $("#readme").html(data);
  });
});

// Button mechanics :
function unlock() {
    var btn = $('#run_btn');
    var btn_text = $('.exec_btn_text');
    var btn_gear = $('.gear');
    var btn_play = $('.play');

    btn.prop('disabled', false)
	.removeClass('disabled')
	.removeClass('btn-outline-success')
	.addClass('btn-success');
    btn_text.text('Run');
    btn_gear.hide();
    btn_play.show();
}

function lock(message) {
    var btn = $('#run_btn');
    var btn_text = $('.exec_btn_text');
    var btn_gear = $('.gear');
    var btn_play = $('.play');

    btn.prop('disabled', true)
	.addClass('disabled')
	.addClass('btn-outline-success')
	.removeClass('btn-success');
    btn_text.text(message);
    btn_gear.show();
    btn_play.hide();
}

function newdoc() {
    editor.setValue("");
}

function load(name) {
    //console.log("loading " + name);
    $.get("examples/" + name + ".mlts", function(data) {
	editor.setValue(data);
    });
}

function save() {
    var uri = 'data:text/octet-stream;charset=utf-8;base64,' +
	btoa(editor.getValue());
    saveAs(uri, "main.mlts");
}

function saveAs(uri, filename) {
  var link = document.createElement('a');
  if (typeof link.download === 'string') {
    link.href = uri;
    link.download = filename;

    //Firefox requires the link to be in the body
    document.body.appendChild(link);
    
    //simulate click
    link.click();

    //remove the link when done
    document.body.removeChild(link);
  } else {
    window.open(uri);
  }
}


// We run Elpi in a separate worker
var elpi = new Worker('js/elpi-worker.js');

function restart() {
    lock('Restarting');
    elpi.terminate();
    elpi = new Worker('js/elpi-worker.js');
    elpi.onmessage = function (event) {
    if(event.data.type == 'ready') {
	unlock();
    }
    else show_resultas(event.data.output);
}
}

elpi.onmessage = function (event) {
    if(event.data.type == 'ready') {
	unlock();
    }
    else show_resultas(event.data.output);
}
var ping = function() {
    elpi.postMessage("ping");
}

// Binding Execute button :
function run() {
    lock('Running');
    var mltsCode = editor.getValue();
    elpi.postMessage(mltsCode);
}

function show_resultas(results) {
    $('#answer').html('');
    results.reverse().forEach(function(res, id) {
	var row = $('<tr></tr>');
	row.append($('<th></th>').attr('scope', 'row').text(id));
	row.append($('<td></td>').text(res.name));
	row.append($('<td></td>').text(res.value));
	$('#answer').append(row);
    });
    $('#myTab a[href="#values"]').tab('show');

    unlock();
}
