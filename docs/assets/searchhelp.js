
$(function() {
    $('#mysearch').on('search', function() {
        var needle = $(this).val();
  	$('.eentry').hide().filter(function (i, e) {
            if (needle == '') return true;
            return  $(e).find('a[title*="'+ needle +'"]').length;
        }).show();
    })
});


$(function() {
    $("#core_p_trigger").click(function() {
        $("#alsdev_trigger").css("background-color", "white");
        $("#alslib_trigger").css("background-color", "white");
        $("#c_intf_trigger").css("background-color", "white");
	$( "#sb_toc" ).load( "toc_core_prolog.html" );
	$("#core_p_trigger").css("background-color", "LightBlue");
   });
});

	// Doesn't fire:
$( "#core_p_trigger" ).trigger( "click" );


$(function() {
    $("#alsdev_trigger").click(function() {
        $("#core_p_trigger").css("background-color", "white");
        $("#alslib_trigger").css("background-color", "white");
        $("#c_intf_trigger").css("background-color", "white");
	$( "#sb_toc" ).load( "toc_alsdev.html" );
	$("#alsdev_trigger").css("background-color", "LightBlue");
   });
});

$(function() {
   $("#alslib_trigger").click(function() {
        $("#core_p_trigger").css("background-color", "white");
        $("#alsdev_trigger").css("background-color", "white");
        $("#c_intf_trigger").css("background-color", "white");
	$( "#sb_toc" ).load( "toc_alslib.html" );
	$("#alslib_trigger").css("background-color", "LightBlue");
   });
});

$(function() {
   $("#c_intf_trigger").click(function() {
        $("#core_p_trigger").css("background-color", "white");
        $("#alsdev_trigger").css("background-color", "white");
        $("#alslib_trigger").css("background-color", "white");
	$( "#sb_toc" ).load( "toc_c_intf.html" );
	$("#c_intf_trigger").css("background-color", "LightBlue");
   });
});

