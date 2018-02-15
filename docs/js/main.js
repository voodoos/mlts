
// Initializing editor
var editor = ace.edit("editor");
editor.setTheme("ace/theme/ambiance");
editor.session.setMode("ace/mode/ocaml");

// Binding Execute button :
$('#run_btn').click(function() {
    $('#lpl').val(compile(editor.getValue()));
    $('#answer').text(run().toString());
});
