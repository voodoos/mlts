
// Initializing editor
var editor = ace.edit("editor");
editor.setTheme("ace/theme/ambiance");
editor.session.setMode("ace/mode/ocaml");



// We run Elpi in a separate worker
var elpi = new Worker('js/elpi-worker.js');
elpi.onmessage = function (event) {
    show_resultas(event.data.output);
}
var ping = function() {
    elpi.postMessage("ping");
}

// Binding Execute button :
$('#run_btn').click(function() {
    var mltsCode = editor.getValue();
    elpi.postMessage(mltsCode);
    /*
    $('#lpl').val(compile(mltsCode));
    var raw = run();
    //console.log("raw: " + raw);
    var res = JSON.parse(raw);
    //console.log("stringified: " + JSON.stringify(res));
    show_resultas(res.output);
    //$('#answer').text(.toString());*/
});

function show_resultas(results) {
    $('#answer').html('');
    results.reverse().forEach(function(res, id) {
	var row = $('<tr></tr>');
	row.append($('<th></th>').attr('scope', 'row').text(id));
	row.append($('<td></td>').text(res.name));
	row.append($('<td></td>').text(res.value));
	$('#answer').append(row);
    });

}
