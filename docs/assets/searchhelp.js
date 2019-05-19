
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
	$( "#sb_toc" ).load( "toc_core_prolog.html" );
   });
});

/*
$(function() {
   $("#core_p_trigger").click(
	$("#sb_toc").load( "toc_core_prolog.html" ));
});
*/

/*
$(function() {
   $("#core_p_trigger").click(function() {
	alert("toc_core_prolog.html");
   });
});
*/
