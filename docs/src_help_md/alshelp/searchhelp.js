
// Trying just to find any entry with title containing "checks" ($exported_proc/3 starts with "checks"):
$(function() {
	$('#mysearch').on('search', function() {
	$('.eentry').hide().filter(".eentry.find('.a[title~=checks'])").show();
	})
});

// This works, including collapsing unselected entries (<li>'s),
// but of course doesn't search the whole title (prev: ix) attribute, 
// only the link content:
//	$('.eentry').hide().filter('.eentry:contains("' + $(this).val() + '")').show();

