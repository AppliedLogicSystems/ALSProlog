
$(function() {
	$('#mysearch').on('search', function() {
    var needle = $(this).val();
  	$('.eentry').hide().filter(function (i, e) {
      if (needle == '') return true;
      return  $(e).find('a[title*="'+ needle +'"]').length;
    }).show();
	})
});

