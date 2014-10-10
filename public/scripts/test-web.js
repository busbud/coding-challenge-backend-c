function getLocation() {
    if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(showPosition);
    } else { 
        document.getElementById("location").innerText = "Geolocation is not supported by this browser.";
    }
}

function showPosition(position) {
    document.getElementById("location").innerText="Latitude: " + position.coords.latitude + "; Longitude: " + position.coords.longitude;
    $("#latitude").val(position.coords.latitude);
    $("#longitude").val(position.coords.longitude);
}

var transform = {'tag':'li','html':'${name} (latitude=${latitude}, longitude=${longitude}, score=${score}, distance=${distance})'};
function loadSuggestions() {
	if ($("#city").val().trim() == '') {
		document.getElementById('suggestions').innerHTML = "";
	} else {
		var url = API_URL + "/suggestions?q=" + $("#city").val() + '&latitude=' + $("#latitude").val() + '&longitude=' + $("#longitude").val();
		var myJsonObj = getJson(url);
		var json = json2html.transform(myJsonObj.suggestions,transform);
		document.getElementById('suggestions').innerHTML = json;
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
				loadSuggestions();
			}
		});
	});
});
