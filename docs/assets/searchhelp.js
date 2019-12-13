$.fn.filterPreds = function(str) {
            console.log('str = ' + str);
        // Usually iterate over the items and return for chainability:
        return this.each(function() {
             // do something to each item matching the selector
            console.log($(this).text());

	   var str2 = '\^' + str;
	   var rx = new RegExp(str2);

            console.log('match str = ' + $(this).text().match(str));
            console.log('match str2 = ' + $(this).text().match(str2));
            console.log('match rx = ' + $(this).text().match(rx));

           if ($(this).text().match(rx) != null) {
                console.log('IF=' + $(this).text())
                $(this).css("background-color", "LightCyan")
           } else {
                console.log('ELSE=' + $(this).text())
//                $(this).hide()
                $(this).css("background-color", "yellow")

           }
        });
};

function doSearch(){
        var str = $("#mysearchvalue").val();
        window.console.log("target=" + str);
	var asides = $("aside");
	
	var As = $(".prd");
	As.myfunction();
	As.filterPreds(str);
};

function doTest(){
        var str = $("#mysearchvalue").val();
        window.console.log("target=" + str);
        var As = $(".funny");
console.log(As);
        As.filterPreds(str);
};

$.fn.myfunction = function () {
    console.log('L='+this.length);
};

$.fn.myfunction2 = function (color) {
    console.log(this.length);
//    console.log(this.eq(2).text);
 //   this.css( "background-color", color );
	$.each(this,function(index, value){alert(index + " = " + value + " | " + value.html)});
};

