function getLocation() {
    if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(showPosition);
    } else { 
        document.getElementById("location").innerText = "Geolocation is not supported by this browser.";
    }
}

function showPosition(position) {
    document.getElementById("location").innerText="Your Latitude: " + position.coords.latitude + "; Longitude: " + position.coords.longitude;
    $("#latitude").val(position.coords.latitude);
    $("#longitude").val(position.coords.longitude);
}

var transform = {'tag':'li','html':'<b>${name}</b> (<span>latitude</span>: ${latitude}, <span>longitude</span>: ${longitude}, <span>score</span>: ${score}, <span>distance</span>: ${distance})'};
function loadSuggestions() {
	if ($("#city").val().trim() == '') {
		document.getElementById('loading').innerHTML = 'No results.';
		document.getElementById('suggestions').innerHTML = '';
	    document.getElementById("numresults").innerText='Number of Results = 0.';
	} else {
		var url = API_URL + "/suggestions?q=" + $("#city").val() + '&latitude=' + $("#latitude").val() + '&longitude=' + $("#longitude").val();
		document.getElementById('loading').innerHTML = 'Loading...';
		var startTime = new Date().getTime(); // report execution time
		var myJsonObj = getJson(url);
		var endTime = new Date().getTime();
		var executionTime = endTime - startTime;
		if (myJsonObj.suggestions.length == 0) {
			document.getElementById('loading').innerHTML = 'No results for ' + $("#city").val();
			document.getElementById('suggestions').innerHTML = '';
		    document.getElementById("numresults").innerText="Number of Results = 0.";
		} else {
			var json = json2html.transform(myJsonObj.suggestions,transform);
			document.getElementById('suggestions').innerHTML = json;
			document.getElementById('loading').innerHTML = '';
		    document.getElementById("numresults").innerText="Number of Results = " + myJsonObj.suggestions.length + 
		    	'; Data loaded in ' + executionTime + ' milliseconds.';
		}
	}
}

function getJson(url) {
 return JSON.parse($.ajax({
     type: 'GET',
     url: url,
     dataType: 'json',
     global: false,
     async:false,
     success: function(data) {
         return data;
     }
 }).responseText);
}

$(document).ready(function() {
	getLocation();
	$("#butt").click(function(){
		loadSuggestions();
	});
	$('#city').on("keypress", function (e) {
	    if (e.keyCode == 13) {
		loadSuggestions();
	    }
	});
	$('#city').each(function() {
		var elem = $(this);
		// Save current value of element
		elem.data('oldVal', elem.val());
		// Look for changes in the value
		elem.bind("propertychange keyup input paste", function(event){
			// If value has changed...
			if (elem.data('oldVal') != elem.val()) {
				// Updated stored value
				elem.data('oldVal', elem.val());
				setTimeout(undefined,5);
				loadSuggestions();
			}
		});
	});
});
