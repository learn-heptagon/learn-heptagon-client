function setEditorSingleLine(editor) {
    /* Previous implementation (which was not working)
       editor.renderer.setShowGutter(false);
       editor.setOption("maxLines", 1);
    */

    editor.setOptions({
        maxLines: 1,
        autoScrollEditorIntoView: true,
        highlightActiveLine: false,
        printMargin: false,
        showGutter: false,
    });

    // Remove newlines in pasted text
    editor.on("paste", function(e) {
        e.text = e.text.replace("/[\r\n]+/g", " ");
    });

    // Safety measure to keep the cursor within the editors visible bounds
    editor.renderer.screenToTextCoordinates = function(x, y) {
        var pos = this.pixelToScreenCoordinates(x, y);
        return this.session.screenToDocumentPosition(
            Math.min(this.session.getScreenLength() - 1, Math.max(pos.row, 0)),
            Math.max(pos.column, 0)
        );
    };

    // Disable Enter and Shift-Enter keys
    editor.commands.bindKey("Enter|Shift-Enter", "null")
}
function setEditorHeight(editor) {
    editor.setOption('maxLines', 30);
}
function setEditorMinHeight(editor) {
    editor.setOption('minLines', 30);
}
function clearEditorSelection(editor) {
    editor.clearSelection();
}
function highlightLine(editor, line, isValid) {
    if (isValid) {
        editor.session.addGutterDecoration(line - 1, "valid-decoration");
    } else {
        editor.session.addGutterDecoration(line - 1, "invalid-decoration");
    }
}
function clearAllHighlights(editor) {
    const lastRow = editor.session.getLength();
    for (let row = 0; row < lastRow; row++) {
        editor.session.removeGutterDecoration(row, "valid-decoration");
        editor.session.removeGutterDecoration(row, "invalid-decoration");
    }
}
