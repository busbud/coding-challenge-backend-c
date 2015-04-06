// helper script to be parse tsv file and transform it into json

var bbp = require('babyparse');
var fs = require('fs');
var path = require('path');
var _ = require('underscore');
var trie_format_string = require('../utils/string').trie_format_string

fs.readFile(path.resolve(__dirname, '../data/cities_canada-usa.tsv'), 'utf8', function(err, tsv_data) {
   
    if (err) {
        console.log('Error reading file', err);
        throw err
    }
    
    console.log('parsing tsv into some nice JSON');
    var parsed = bbp.parse(tsv_data, {
        header: true, 
        dynamicTyping: true, 
        fastMode: true
    });


    // we're inserting into our city_info map with the key as the city name 
    // trie-formatted to make it easier to reconcile our data with the Trie.
    // we keep track of a city's real full name with the 'name' attr in
    // the obj itself.
    // i.e. one city_info datapoint will look like:
    //
    //     trie_format_string(city_name): {
    //         name: city_name,
    //         lat: latitude,
    //         long: longitude,
    //         country: country_code
    //         admin1: admin_code
    //     }
    //
    console.log('constructing city_info map');
    var city_info = _.reduce(_.filter(parsed.data, function(city) {
        return city.population >= 5000;
    }), function(obj, city) {
        var city_name = trie_format_string(city.name);
        var picked_city_data = _.pick(city, 'name', 'lat', 'long', 'country', 'admin1');
        
        if (obj[city_name]) {
            obj[city_name].push(picked_city_data);
        } else {
            obj[city_name] = [picked_city_data];
        }
        return obj;
    }, {});
    
    fs.writeFile(path.resolve(__dirname, '../data/city_info.json'), JSON.stringify(city_info), function(err) {
        if (err) {
            console.log('Error saving file');
            throw err
        }
        console.log('Saved city_info.json');
    });
});
