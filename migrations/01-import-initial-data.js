var Importer = require('geonames-importer');

var admin1CodesToProvinceCode = {
    "01": "AB",
    "02": "BC",
    "03": "MB",
    "04": "NB",
    "05": "NL",
    "07": "NS",
    "08": "ON",
    "09": "PE",
    "10": "QC",
    "11": "SK",
    "12": "YT",
    "13": "NT",
};

var importer = new Importer({
    filename: __dirname + '/../data/cities_canada-usa.tsv',
    index: process.env.ES_INDEX || "locations",
    elasticsearch : {
        host: process.env.ES_HOST || "192.168.99.100:32771"
    },
    transformers: [
        function (item) {
            item.location = {
                lat: item.latitude,
                lon: item.longitude
            };

            if(item.countryCode == "CA") {
                var admin1Code = item.admin1Code;
                item.admin1Code = admin1CodesToProvinceCode[admin1Code];
            }

            delete item.latitude;
            delete item.longitude;
            return item;
        },
    ]
});

importer
.import()
.then(function () {
    console.log('finished');
})
.done();

//TODO: DIACRITICS

/*curl -X DELETE  http://192.168.99.100:32771/locations*/


/*curl -X PUT  http://192.168.99.100:32771/locations/ -d'{
    "settings":{
        "analysis":{
            "analyzer":{
                "autocomplete":{
                    "tokenizer":"autocomplete",
                    "filter":[
                        "lowercase"
                    ]
                },
                "autocomplete_search":{
                    "tokenizer":"lowercase"
                }
            },
            "tokenizer":{
                "autocomplete":{
                    "type":"edge_ngram",
                    "min_gram":2,
                    "max_gram":10,
                    "token_chars":[
                        "letter"
                    ]
                }
            }
        }
    },
    "mappings":{
        "geoname":{
            "properties":{
                "admin1Code":{
                    "type":"text",
                    "analyzer":"autocomplete",
                    "search_analyzer":"autocomplete_search",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "admin2Code":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "admin3Code":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "admin4Code":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "alternateNames":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "asciiName":{
                    "type":"text",
                    "analyzer":"autocomplete",
                    "search_analyzer":"autocomplete_search",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "cc2":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "countryCode":{
                    "type":"text",
                    "analyzer":"autocomplete",
                    "search_analyzer":"autocomplete_search",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "dem":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "elevation":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "featureClass":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "featureCode":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "id":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "location":{
                    "type":"geo_point"
                },
                "modificationDate":{
                    "type":"date"
                },
                "name":{
                    "type":"text",
                    "analyzer":"autocomplete",
                    "search_analyzer":"autocomplete_search",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                },
                "population":{
                    "type":"long"
                },
                "timezone":{
                    "type":"text",
                    "fields":{
                        "keyword":{
                            "type":"keyword",
                            "ignore_above":256
                        }
                    }
                }
            }
        }
    }
}'*/

