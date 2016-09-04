var mongoose = require('mongoose');

var City = mongoose.model('City', {

    // geonameid
    id : Number,

    //name of geographical point (utf8) varchar(200)
    name : String,

    //ascii name of geographical point in plain ascii characters, varchar(200)
    ascii : String,

    // alternatenames, comma separated varchar(5000)
    alt_name    : String,

    // latitude in decimal degrees (wgs84)
    latitude : Number,

    // longitude in decimal degrees (wgs84)
    longitude : Number,

    coords : { type: [Number], index: '2dsphere' },

    // see http://www.geonames.org/export/codes.html, char(1)
    feat_class : String,

    // see http://www.geonames.org/export/codes.html, varchar(10)
    feat_code : String,

    // ISO-3166 2-letter country code, 2 characters
    country : String,

    // alternate country codes, comma separated, ISO-3166 2-letter country code, 60 characters
    cc2 : String,

    // fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display
    // names of this code; varchar(20)
    admin1 : String,

    // code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80)
    admin2 : String,

    // code for third level administrative division, varchar(20)
    admin3 : String,

    // code for fourth level administrative division, varchar(20)
    admin4 : String,

    // bigint (8 byte int)
    population : Number,

    // in meters, integer
    elevation : Number,

    // digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30''
    // (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
    dem : Number,

    // the timezone id (see file timeZone.txt) varchar(40)
    timezone : String,

    // date of last modification in yyyy-MM-dd format
    modified_at : { type: Date },

    // The country String
    countryLong : String,

    // The area name of the city
    areaLong : String,

    // The area code of the city
    areaCode : String

});

City.collection.ensureIndex({ name: "text", alt_name : "text" });


module.exports = City;