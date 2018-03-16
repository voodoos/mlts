var left = $(".main-wrapper .left");
var right = $(".main-wrapper .right");

function isDragable() {
    return $('#scroll_handle').is(':visible');
}

function offDrag() {
    if (!isDragable()) {
	left.css('max-width', '')
	    .css('flex-basis', '');
	right.css('max-width', '')
	    .css('flex-basis', '');
	$('#scroll_handle')
	    .css('left', '50%');
    }
}
$(window).bind("resize", offDrag);

var $draggable = $('#scroll_handle').draggabilly({
    axis: 'x',
    containment: true
})


function setProp(prop) {
    var p = Math.max(Math.min(prop, 100), 0);
    var ip = 100 - p;

    left.css('max-width', p + "%")
	.css('flex-basis', p + "%");
    right.css('max-width', ip + "%")
	.css('flex-basis', ip + "%");;
}

function listener(/* parameters */) {
  // get Draggabilly instance
    var draggie = $(this).data('draggabilly');
    
    var mw = $(".row").width();
    var prop = 100 * (draggie.position.x
		      + ((mw - $( window ).width())
			 /2))
	/ mw;
    
    setProp(prop);
    //console.log( 'eventName happened', prop, mw);
}

// bind event listener
$draggable.on( 'dragMove', listener );
