/* inspirations, stumbled upon...
https://gist.github.com/justinvw/5025854 -- es_simple_autocomplete_example_config.sh
http://stackoverflow.com/questions/26005449/elasticsearch-sort-on-multiple-queries -- Sorting ES results based on multiple queries
*/

var http = require('http');
var express = require('express');
var app = express();
var port = process.env.PORT || 3456;
var _ = require('underscore');
var async = require('async')

var elasticsearch = require('elasticsearch');
var client = new elasticsearch.Client({
    // Put your own host. I found a free ES cloud instance at qbox.io
  host: 'http://15736341b24c13ac000.qbox.io/',
  //log: 'trace'
});

// express settings
require('./config/express.js')(app);

// API endpoint.  If there were more routes, I'd put it in a route folder and init them instead.
app.route('/suggestions').get(function(req, res) {
    var query = {
        bool :{
            must : [
            {
                constant_score: {
                    boost: 1,
                    query: {
                        match : {
                            name: {
                                query: req.query.q,
                                analyzer: 'standard'
                            }
                        }
                    }
                }
            }
            ]
        }
        
    };

    // if there is a location parameters, adjust the query.
    if (!!req.query.latitude && !!req.query.longitude && !false)
        query.bool.must.push({
            function_score: {
                functions: [{
                    exp: {
                        point: {
                            origin: {
                                lat: Number(req.query.latitude),
                                lon: Number(req.query.longitude)
                            },
                            offset: '150km',
                            scale: '150km'
                        }
                    }
                }]
            }
        });
    
    client.search({
        index: 'cities',
        body: {
            query: query
        }
    }, function (error, response) { 
        if (error) console.log(error);
        if (response.hits.total > 0) {
            var suggestions = response.hits.hits;
            suggestions = _.map(suggestions, function(s) {
                return {
                    score: s._score / response.hits.max_score,
                    name: s._source.display,
                    latitude: s._source.point.lat,
                    longitude: s._source.point.lon
                }
            })
        
            res.json({ suggestions: suggestions })
        } else res.status(404).json({ suggestions: [] })
    });

});


var server = http.createServer(app);

async.waterfall([
    function deleteIndex(callback){
        // Uncomment this line to skip the whole seeding/prepping of ES instance
        //return callback('skip');
        console.log("Find this comment in the code to skip all this process.");

        console.log("Deleting the index 'cities'")
        client.indices.delete({ index: 'cities', ignore: [404] }, function (error, response) {
            callback(error);
        });
    },    
    function create(callback){
        console.log("Creating the index 'cities'")
        client.indices.create({ index: 'cities' }, function (error, response) {
            callback(error);
        });
    },    
    function close(callback){
        console.log("Closing the index 'cities'")
        client.indices.close({ index: 'cities' }, function (error, response) {
            callback(error);
        });
    },
    function putSettings(callback){
        console.log("Changing settings in the index 'cities' (analyzer and filters)")

        client.indices.putSettings({ 
            index: 'cities',
            body: {
                index: {
                    "analysis" : {
                        "analyzer" : {
                            "autocomplete_analyzer" : {
                                "type" : "custom",
                                "tokenizer" : "lowercase",
                                "filter"    : ["asciifolding", "title_ngram"]
                            }
                        },
                        "filter" : {
                            "title_ngram" : {
                                "type" : "edgeNGram",
                                "min_gram": 3,
                                "max_gram" : 20
                            }
                        }
                    }
                }
            }
        }, function (error, response) {
            callback(error);
        });
    },
    function putMapping(callback){
        console.log("Changing mapping in the index 'cities'")
        client.indices.putMapping({ 
            index: 'cities',
            type: 'city',
            body: { 
                "properties" : {
                    name : { 
                        type : "string",
                        "analyzer": "autocomplete_analyzer",
                        "boost": 15
                    },
                    point :{
                        type: "geo_point",
                        lat_lon: true
                    }
                }
            }
        }, function (error, response) {
            callback();
        });
    },
    function open(callback){
        console.log("Reopening the index 'cities'");
        client.indices.open({ index: 'cities' }, function (error, response) {
            callback(error);
        });
    },
    function seed(callback) {
        console.log("Seeding the ES instance with the cities_canada-usa.tsv data... Might take a few moments.")

        var fs = require('fs');

        fs.readFile('data/cities_canada-usa.tsv', function (err, data) {
            if (err) console.log (err);

            data = String(data).split("\n");
            data = _.map(data, function (d) {
                return d.split("\t");
            });

            // first row is the keys
            var keys = _.first(data); 

            // making an array of objects with proper keys-values
            data = _.map(_.rest(data), function (d) {
                var city = {};
                for (var i = 0; i < keys.length; i++) {
                    city[keys[i]] = d[i];
                };
                return city;
            });

            // indexing
            async.each(data, function(city, cb) {
                client.index({
                    index: 'cities',
                    type: 'city',
                    id: city.id,
                    body: {
                        name: city.name,
                        display: city.name + ', ' + city.admin1 + ', ' + city.country,
                        point: {
                            lat: Number(city.lat),
                            lon: Number(city.long)
                        }
                    }
                }, function (error, response) {
                    cb();
                });

            }, function (err) {
                callback();
            })
        })
}], 
function search(err, result) {
    if (err) console.log(err);
    else {
        console.log("Express server started on port " + port);
        console.log ("Okay, ready to give suggestions.");
    }
});

server.listen(port);
exports = module.exports = app
