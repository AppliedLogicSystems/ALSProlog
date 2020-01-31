var switchpoint = 2;
var rgbWhite = "rgb(255, 255, 255)";
var rgbWhiteA = "rgba(0, 0, 0, 0)";

function accessPredPage(page, pred) 
{
    var localSearchWhere = $("input[name='searchtype']:checked").val();
    var srchVal = $("#searchInput").val();
    var cleanp;

    var pred0 = pred.charAt(0);
    if (pred0 === "$") { 
	cleanp = pred.slice(1); 
    } else {
	cleanp = pred;
    }

    var aid = "#"+page+"_"+encodeURIComponent(cleanp);
    var initurl = $(aid).attr('href');

    var xurl = initurl;
    if (srchVal  === '') {
        xurl = xurl + "?stype=" + localSearchWhere + "&stgt=";
    } else {
        xurl = xurl + "?stype=" + localSearchWhere + "&stgt=" + srchVal;
    }
    $(aid).attr('href',xurl)
}

function setupRef()
{
    var locarray = window.location.href.split("?");
    if (locarray.length > 1) {
	  var qs = locarray[locarray.length - 1];
	  
          var qsarray = qs.split("&");
	  var stypeeq = qsarray[0];
	  var searchtermeq = qsarray[1];

	  var searchtype = stypeeq.split("=")[1];
	  $("#"+searchtype).prop( "checked", true );

	  var searchterm = decodeURIComponent(searchtermeq.split("=")[1]);
	  $("#searchInput").val(searchterm);
    } 
    doSearch();
}


$.fn.filterPreds = function(str, radioVal) {
        	// Usually iterate over the items and return for chainability:
        return this.each(function() {
             	// do something to each item matching the selector
	   var plain_x = new RegExp(str);
	   var front_str = '\^\\' + str;
	   var front_x = new RegExp(front_str);


	   if (radioVal == "srchboth") {
	       if (($(this).find("a").text().trim().match(plain_x) != null) ||
                     ($(this).find(".dscr").text().trim().match(plain_x) != null) ) 
	       {
                    $(this).find("a").css("background-color", "LightCyan")
		    $(this).closest(".grp").find("details").find("summary").first().css("background-color", "#ffe2e6")
		    $(this).closest(".pkg").find("details").find("summary").first().css("background-color", "AntiqueWhite")
		    if (str.length > switchpoint) {
		    	$(this).closest(".grp").find("details").attr('open', '')
		    	$(this).closest(".pkg").find("details").attr('open', '')
		    }
               } else {
                    $(this).hide()
               }
	   } else if ((radioVal == "srchdesc")) {
               if ($(this).find(".dscr").text().trim().match(plain_x) != null) 
	       {
                    $(this).find("a").css("background-color", "LightCyan")
		    $(this).closest(".grp").find("details").find("summary").first().css("background-color", "#ffe2e6")
		    $(this).closest(".pkg").find("details").find("summary").first().css("background-color", "AntiqueWhite")
		    if (str.length > switchpoint) {
		    	$(this).closest(".grp").find("details").attr('open', '')
		    	$(this).closest(".pkg").find("details").attr('open', '')
		    }
               } else {
                    $(this).hide()
               }
	   } else {
		str = "^"+str;
	       if ($(this).find("a").text().trim().match(plain_x) != null) 
	       {
                    $(this).find("a").css("background-color", "LightCyan")
		    $(this).closest(".grp").find("details").find("summary").first().css("background-color", "#ffe2e6")
		    $(this).closest(".pkg").find("details").find("summary").first().css("background-color", "AntiqueWhite")
		    if (str.length > switchpoint) {
		    	$(this).closest(".grp").find("details").attr('open', '')
		    	$(this).closest(".pkg").find("details").attr('open', '')
		    }
               } else {
                    $(this).hide()
               }
	   }
        });
};

function doSearch () {
        var str = $("#searchInput").val();
    	var localSearchWhere = $("input[name='searchtype']:checked").val();
	var char0 = str.charAt(0);
	if (char0 == "$") { 
		str = str.slice(1); }
	if (str) {
		var As = $(".prd");
        	As.filterPreds(str, localSearchWhere);
		cleanUp();
	}
};

function cleanUp(){

        $.each($(".grp").find("details"),
                function( index, value ) {
                   var grpbkg = $(value).find("summary").first().css("background-color");
                   if (grpbkg === rgbWhite || grpbkg === rgbWhiteA ){
                        $(value).closest(".grp").find("details").removeAttr("open");
                   }
                }
        )
}

function clearSearch() {
	var As = $(".prd");
	var Bs = $(".grp");
	var Cs = $(".pkg");
	As.show()
	As.find("a").css("background-color", "white")
	Bs.find("details").removeAttr('open').find("summary").css("background-color", "white")
	Cs.find("details").removeAttr('open').find("summary").css("background-color", "white")
	Cs.find("details").first().attr('open', '');
}

