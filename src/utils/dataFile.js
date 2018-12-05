var d3 = require('d3');
const fs = require('fs'); 

var cache;
var file = 'data/cities_canada-usa.tsv';

module.exports.import = 
/**
 * Get all the cities from flat file or cache
 */
function() {

    return new Promise(function(resolve, reject) {

        if (cache !== undefined && cache.length > 0) {
            resolve(cache);
        }

        fs.readFile(file, "utf8", function(error, data) {
            data = data.replace(/"/g, '');            
            data = d3.tsvParse(data);
            cache = data;
            resolve(data);
        });
        
        
   
    });
}