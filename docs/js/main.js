
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

//hljs.configure({useBR: true, languages: ['prolog', 'bash']});



// Initializing tooltips:
$(function () {
  $('[data-toggle="tooltip"]').tooltip()
})

// Loading readme and open file :
$(document).ready(function(){ 
  $.get("readme.html", function(data) {
      $("#readme").html(data);
      /*$('#readme pre').each(function(i, block) {
	    hljs.highlightBlock(block);
	});*/
  });
    
    load(window.location.hash.substring(1));
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
    if (name != '') {
	$.get("examples/" + name + ".mlts", function(data) {
	    editor.setValue(data);
	    editor.clearSelection();
	});
    }
}

function save() {
    var uri = 'data:text/octet-stream;charset=utf-8;base64,' +
	btoa(editor.getValue());
    if (window.location.hash.substring(1) != '') 
	saveAs(uri, window.location.hash.substring(1) + ".mlts");
    else saveAs(uri, "main.mlts");
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
var defs;

function onMessageCB(event) {
    if(event.data.type == 'ready') {
	unlock();
    }
    else if(event.data.type == 'log') {
	$('#log').prepend('<br/>').prepend(event.data.text);
    }
    else if (event.data.type == 'error') {
	unlock();
	
	$('#run_btn').addClass('blinking');
	setTimeout(function(){ $('#run_btn').removeClass('blinking') }, 1000);
	
	if(event.data.line > 0) {
	    editor.gotoLine(event.data.line);
	    $('#editor').addClass('red-alert');
	    setTimeout(function(){ $('#editor').removeClass('red-alert') }, 1000);
	}
    }
    else if(event.data.type == 'lplcode') {
	defs = event.data.defs;
	$('#lpl').html('').append(event.data.code
				  .replace(/arobase/g, '@')
				  .replace(/'/g, '&rsquo;')
				  .replace(/\./g, '.<br>'));
	$('#lpl').each(function(i, block) {
	    hljs.highlightBlock(block);
	});
	$('#myTab a[href="#lpl"]').tab('show');
    }
    else if(event.data.type == 'version') {
	$('#version').html('').append("v"+event.data.n);
    }
    else show_resultas(event.data.output);
}

function restart() {
    lock('Restarting');
    elpi.terminate();
    elpi = new Worker('js/elpi-worker.js');
    elpi.onmessage = onMessageCB;
}

elpi.onmessage = onMessageCB;

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
	var row = $('<tr></tr>').addClass("clickable").click(function(e) { goto_def(decodeURI(res.name)) });
	row.append($('<td></td>').text(decodeURI(res.name)));

	// Some ugly-regex-magic-based pretty printing :
	var color = ((res.value.includes("error")
		      || (res.value.includes("failed"))) ? "red"
		     : "black");
	row.append($('<td></td>').addClass("reslpl").css('color', color)
		   .text(decodeURI(res.value)
			 .replace(/i /g, '')
			 .replace(/tt/g, 'True')
			 .replace(/ff/g, 'False')
			 .replace(/null/g, '[]')
			 .replace(/ab/g, 'Abt')
			 .replace(/ap/g, 'App')
			 .replace(/_[0-9]+/g, '')
			 .replace(/c_/g, '')
			 .replace(/cns \((.*?)\) (.*?)/g,
				  '$1::$2')
			 .replace(/::\((.*::.*)\)/g, '::$1')
			 .replace(/::\((.*::.*)\)/g, '::$1')
			 .replace(/::\((.*::.*)\)/g, '::$1')
			 .replace(/::\((.*::.*)\)/g, '::$1')
			)
		  );
	$('#answer').append(row);
	
    });
    
    $('#myTab a[href="#values"]').tab('show');

    unlock();
}

function get_def_line(name) {
    return ((defs.find(function(element) {
	if (element.length > 2)
	    return (element[1].c) == name;
	else return false
    }))[2])
}

function goto_def(name) {
    editor.gotoLine(get_def_line(name));
    $('#editor').addClass('white-alert');
    setTimeout(function(){ $('#editor').removeClass('white-alert') }, 1000);
}
