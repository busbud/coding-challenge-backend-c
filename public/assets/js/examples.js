$(document).ready(function() {

    window.latitude = 0;
    window.longitude = 0;

    var lastSearch;
    var busbudxjr = new Bloodhound({
        datumTokenizer: Bloodhound.tokenizers.obj.whitespace('value'),
        queryTokenizer: Bloodhound.tokenizers.whitespace,
        remote: {
            url: suggestionpath,

            prepare: function(query, settings) {

                $('#awloading').css('display','inline');
                settings.url = settings.url + '?q=' + query;

                if(window.latitude!=0){
                    settings.url +='&latitude='+window.latitude;
                    settings.url +='&longitude='+window.longitude;
                }

                settings.headers = {
                    'MCA': 'B',
                    'X-Busbud-Token': 'PARTNER_BaASYYHxTxuOINEOMWq5GA',
                };
                settings.beforeSend = function(e) {
                    if (lastSearch) {
                         lastSearch.abort();
                    }
                    lastSearch = e;
                };

                return settings;
            },
            rateLimitBy: 'throttle',
            rateLimitWait: 800
        },
    });



    $('#location .typeahead').bind('typeahead:asyncreceive', function(ev, suggestion,c) {
        $('#awloading').css('display','none');
    });

    $('#location .typeahead').typeahead(null, {
        display: 'name',
        source: busbudxjr,
        templates: {
            empty: function(context){
            //  console.log(1) // put here your code when result not found
              $(".tt-dataset").text('No Results Found');
            }
        }
    });

    if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(markposition);
          $('#permission1').hide();
    }else{
          $('#permission1').show();
    }

    function markposition(position) {
        window.latitude = position.coords.latitude;
        window.longitude = position.coords.longitude;

    }


});
