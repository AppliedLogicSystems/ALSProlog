
function accessPredPage(id) 
{
    var localSearchWhere = $("input[name='searchtype']:checked").val();

    var srchVal = $("#searchInput").val();

    var aid = "#"+id;
    var initurl = $(aid).attr('href');

    var xurl = initurl;
    if (srchVal  === '') {
        xurl = xurl + "?stype=" + localSearchWhere + "&stgt=";
    } else {
        xurl = xurl + "?stype=" + localSearchWhere + "&stgt=" + "\\" + srchVal;
    }

    $(aid).attr('href',xurl)
}

function setupRef()
{
//alert("setupRef: window.location.href=" + window.location.href);

    var locarray = window.location.href.split("?");
    if (locarray.length > 1) {
	  var qs = locarray[locarray.length - 1];
	  
          var qsarray = qs.split("&");
	  var stypeeq = qsarray[0];
	  var searchtermeq = qsarray[1];

	  var searchtype = stypeeq.split("=")[1];
	  $("#"+searchtype).prop( "checked", true );

	  var searchterm = searchtermeq.split("=")[1];
	  searchterm = searchterm.substring(1);
	  $("#searchInput").val(searchterm);
    } 
    doSearch();
}


$.fn.filterPreds = function(str, radioVal) {
        	// Usually iterate over the items and return for chainability:
        return this.each(function() {
             	// do something to each item matching the selector
	   var rx = new RegExp(str);
	   var str2 = '\^' + str;
	   var rx2 = new RegExp(str2);

	   if (radioVal == "srchboth") {
	       if (($(this).text().trim().match(rx) != null) ||
                     ($(this).find(".dscr").text().trim().match(rx2) != null) ) 
	       {
                    $(this).find("a").css("background-color", "LightCyan")
		    $(this).closest(".grp").find("details").find("summary").first().css("background-color", "LightPink")
		    $(this).closest(".pkg").find("details").find("summary").first().css("background-color", "AntiqueWhite")
               } else {
                    $(this).hide()
               }
	   } else if ((radioVal == "srchdesc")) {
	       if ($(this).find(".dscr").text().trim().match(rx2) != null) 
	       {
                    $(this).find("a").css("background-color", "LightCyan")
		    $(this).closest(".grp").find("details").find("summary").first().css("background-color", "LightPink")
		    $(this).closest(".pkg").find("details").find("summary").first().css("background-color", "AntiqueWhite")
               } else {
                    $(this).hide()
               }
	   } else {
	       if ($(this).find("a").text().trim().match(rx) != null) 
	       {
                    $(this).find("a").css("background-color", "LightCyan")
		    $(this).closest(".grp").find("details").find("summary").first().css("background-color", "LightPink")
		    $(this).closest(".pkg").find("details").find("summary").first().css("background-color", "AntiqueWhite")
               } else {
                    $(this).hide()
               }
	   }
        });
};

function doSearch () {
        var str0 = $("#searchInput").val();
	var str = "\\" + str0;
	var localSearchWhere = $("input[name='searchtype']:checked").val();

	var As = $(".prd");
	if (localSearchWhere != window.radioVal ) {
		window.radioVal = localSearchWhere;
        	As.filterPreds(str, localSearchWhere);
	} else {
        	As.filterPreds(str, window.radioVal);
	}
        As.filterPreds(str, window.radioVal);
};

function clearSearch() {
	var As = $(".prd");
	var Bs = $(".grp");
	var Cs = $(".pkg");
	As.show()
	As.find("a").css("background-color", "white")
	Bs.find("details").find("summary").css("background-color", "white")
	Cs.find("details").find("summary").css("background-color", "white")
	Bs.find("details").removeAttr("open");
}
