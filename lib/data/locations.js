var elasticsearch = require('elasticsearch');
var LRU = require('lru-cache');

var GEO_ASSIST_OFFSET = "500km";
var GEO_ASSIST_SCALE = "4000km";
var GEO_ASSIST_DECAY = 0.33;
var GEO_ASSIST_WEIGHT = 5;

var cache = new LRU({ 
    max: 500,
    length: 50000,
    maxAge: 1000 * 60 * 60 
});

function LocationsDAL(options) {

    var client = new elasticsearch.Client({
      host: options.host,
      //log: 'trace'
    });

    this._getCacheKey = function(query) {
        var cacheKey = query.q;

        if(query.longitude !== undefined && query.latitude !== undefined) {
            cacheKey = cacheKey + "_" + Math.round(query.latitude) + "_" + Math.round(query.longitude);
        }

        return cacheKey;
    }

    this.getSuggestions = function(query, callback) {

        query.q = query.q.replace("-", " ");

        var cacheKey = this._getCacheKey(query);

        if(cache.has(cacheKey)) {
            return callback(undefined, cache.get(cacheKey));
        }

        var esQuery = {
            "index" : options.index,
            "type": "geoname",
            "body": {
                "query":{
                    "function_score":{
                        "query":{
                            "multi_match" : {
                                "query": query.q || "", 
                                "fields": [ "name", "asciiName", "countryCode", "admin1Code" ] 
                            }
                        }
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

        client.search(esQuery, function(err, result) {
            if(!err) {
                cache.set(cacheKey, result);
            }
            callback(err, result);
        });
    };

    return this;
};

module.exports = LocationsDAL;