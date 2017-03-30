var elasticsearch = require('elasticsearch');

var GEO_ASSIST_OFFSET = "500km";
var GEO_ASSIST_SCALE = "4000km";
var GEO_ASSIST_DECAY = 0.33;
var GEO_ASSIST_WEIGHT = 5;

function LocationsDAL(options) {

    var client = new elasticsearch.Client({
      host: options.host,
      //log: 'trace'
    });

    this.getSuggestions = function(query, callback) {

        var esQuery = {
            "index" : options.index,
            "type": "geoname",
            "body": {
                "query":{
                    "function_score":{
                        "query":{
                            /*"bool": {
                                "should": {
                                  "prefix": {
                                    "name": query.q.replace("-", " ")
                                  }
                                },
                                "should": {
                                  "prefix": {
                                    "asciiName": query.q.replace("-", " ")
                                  }
                                },
                                "should": {
                                  "prefix": {
                                    "countryCode": query.q.replace("-", " ")
                                  }
                                },
                                "should": {
                                  "prefix": {
                                    "admin1Code": query.q.replace("-", " ")
                                  }
                                }
                              }*/

                            "multi_match" : {
                                "query": query.q.replace("-", " ") || "", 
                                "fields": [ "name", "asciiName", "countryCode", "admin1Code" ] 
                            }
                            /*"query_string": {
                              "fields": [
                                "name",
                                "asciiName",
                                "countryCode",
                                "admin1Code"
                              ],
                              "query": (query.q.replace("-", " ") || "") + "*"
                            */                      }
                    }
                }
            }
        };
        
        if(query.longitude !== undefined && query.latitude !== undefined) {
            esQuery.body.query.function_score.functions = [
                  {
                    "gauss":{
                        "location":{
                            "origin":{
                                "lat": parseFloat(query.latitude),
                                "lon": parseFloat(query.longitude)
                            },
                            "offset": GEO_ASSIST_OFFSET,
                            "scale": GEO_ASSIST_SCALE,
                            "decay": GEO_ASSIST_DECAY
                        }
                    },
                    "weight": GEO_ASSIST_WEIGHT
                }
            ]
        }

        client.search(esQuery, callback);
    };

    return this;
};

module.exports = LocationsDAL;