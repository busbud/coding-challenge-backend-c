// Watch for tickbox change. Verify navigator is enabled when it's checkced.
$('#latLonBox').change(function() {
  if (this.checked) {
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(function() {
        console.log('LatLon Enabled');
      });
    }

  } else {
    console.log('LatLon Disabled');
  }
});

// Builds URL to query. Checks if lat/lon is enabled and appends to the URL if so.
// Currently in order to search, user must enter a minimum of two characters.
// Delay on search after last keypress is 200ms.
$('#searchBox').autocomplete({
  source: function(request, response) {
    var url = '/suggestions?q=' + request.term;

    if (navigator.geolocation && $('#latLonBox').is(':checked')) {
      navigator.geolocation.getCurrentPosition(function(position) {
        var coords = position.coords;
        url = url + '&latitude=' + coords.latitude + '&longitude=' + coords.longitude;
        searchSuggestions(url, response);
      });

    } else {
      searchSuggestions(url, response);
      if (!navigator.geolocation) {
        console.log('Geolocation is not supported by this browser.');
      }
    }
  },

  minLength: 2,
  delay: 200,

  select: function(event, ui) {
    $('#subtitle').text('Hooray! You selected ' + ui.item.value);
  }
});

// Ajax query, map results, and return.
var searchSuggestions = function(url, response) {
  $.ajax({
    dataType: 'json',
    type: 'Get',
    url: url,

    success: function(data) {
      response($.map(data.suggestions, function(item) {
        return item['name'];
      }));
    },

    error: function(data) {
      response(['Oops, there seems to be an issue loading data!']);
    }
  });
};
