//Imports
var jsonfile = require('jsonfile');
var lineReader = require('line-reader');

/**
 * Source file (dump from http://download.geonames.org/export/dump/cities5000.zip)
 * all cities with a population > 5000
 */ 
// Import dumpfile from geonames.org 
// var sourceFilePath='./data/cities/cities5000.txt';
// Import from TSV file furnished by busbud
var sourceFilePath='./data/cities_canada-usa.tsv';

// Variable to store datas we will export as json file
var myJson = [];

// Codes association used by geonames.org for Canadian Provinces
var provinces = {
    '01': 'AB','02': 'BC','03':'MB','04': 'NB','05': 'NL','07': 'NS',
    '13': 'NT','14': 'NU','08': 'ON','09': 'PE','10': 'QC','11': 'SK',
    '12': 'YT'
};

// We parse each line of the dump file
lineReader.eachLine(sourceFilePath, function(line, last) {

  // First filter : reject lines that won't match country code for US & Canada
  if( line.indexOf('CA') !== -1 || line.indexOf('US') !== -1){

    // Explode the line to get only useful properties for us
    var explode = line.split('\t');
    
    /**
     * Schema used by geonames.org : 
     * 0: geonameid         : integer id of record in geonames database
     * 1: name              : name of geographical point (utf8) varchar(200)
     * 2: asciiname         : name of geographical point in plain ascii characters, varchar(200)
     * 3: alternatenames    : alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
     * 4: latitude          : latitude in decimal degrees (wgs84)
     * 5: longitude         : longitude in decimal degrees (wgs84)
     * 6: feature class     : see http://www.geonames.org/export/codes.html, char(1)
     * 7: feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
     * 8: country code      : ISO-3166 2-letter country code, 2 characters
     * 9: cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
     * 10: admin1 code       : fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
     * 11: admin2 code       : code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80) 
     * 12: admin3 code       : code for third level administrative division, varchar(20)
     * 13: admin4 code       : code for fourth level administrative division, varchar(20)
     * 14: population        : bigint (8 byte int) 
     * 15: elevation         : in meters, integer
     * 16: dem               : digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
     * 17: timezone          : the iana timezone id (see file timeZone.txt) varchar(40)
     * 18: modification date   
     */

    // If this city is in US or Canada we will store it
    if(explode[8] === 'CA' || explode[8] === 'US'){

        // Create our object for this city
        var lineObj = {
            name: explode[1],
            latitude: explode[4],
            longitude: explode[5],
            countryCode: explode[8]
        };

        //  If city is in Canada we use the geonames code association to get province iso code,
        // If city is in US we just use directly state iso code
         lineObj.stateCode = (explode[8] === 'CA')? provinces[explode[10]] : explode[10];

        // Save current city parsed in our json vaiable
        myJson.push(lineObj);
    }

  }

  if(last){

    //When all file is parsed we save our json data in a json file 
    jsonfile.writeFile('./cities/usa-ca.json', myJson, function (err) {
        if(err) {
            console.log('Error occured saving file : ');
            console.log(err);
        }
        console.log('end');
    })    
  }
});
