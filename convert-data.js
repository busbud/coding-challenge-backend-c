var fs = require("fs");
var es = require("event-stream");
var vsprintf = require("sprintf").vsprintf;

module.export = fs.createReadStream("./data/cities_canada-usa.tsv")
.pipe(es.replace(",","\,"))
.pipe(es.replace('\"','\\"'))
.pipe(es.split("\n"))
.pipe(es.mapSync(function(data) {
    return data.split("\t");
}))
.pipe(es.mapSync(function(data) {
  return vsprintf(
    '{ "geonameid" : "%1$s","name" : "%2$s","asciiname" : "%3$s","alternatenames" : "%4$s","latitude" : "%5$s","longitude" : "%6$s","featureclass" : "%7$s","featurecode" : "%8$s","countrycode" : "%9$s","cc2" : "%10$s","admin1code" : "%11$s","admin2code" : "%12$s","admin3code" : "%13$s","admin4code" : "%14$s","population" : "%15$s","elevation" : "%16$s","dem" : "%17$s","timezone" : "%18$s","modificationdate" : "%19$s"  }'
    , data, 'j');
}))
.pipe(es.join(",\r"))
.pipe(es.wait())
.pipe(es.mapSync(function(data) {
    return '{ "cities" : [' +  data  + "] }";
}))
.pipe(fs.createWriteStream("data/cities_canada-usa.json"));