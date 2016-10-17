var fs = require('fs');
var csvParse = require('csv-parse/lib/sync');
var City = require('./models/cities');

// Do read file synchronously
exports.readFile = function(){
  try{
    // TSV parser (Found in the web)
    var tsvData = fs.readFileSync('./data/cities_canada-usa.tsv');
    var data = csvParse(tsvData, {
       columns: true,
       delimiter: '\t',
       quote:'\u0000',
       skip_empty_lines: true
    });
    // Construct the basic object out of the City schema
    data.forEach(function(row, index){
      var city = new City({
        id : row.id,
        ascii : row.ascii,
        lat: row.lat,
        long: row.long,
        country: row.country
      });
      // Canadian State is not defined as 2 letters
      if(row.admin1){
        if(city.country === 'CA')
          city.state = getCanadianState(row.admin1);
        else if(city.country === 'US')
          city.state = row.admin1;
      }
      if(row.name){
        city.name = row.name;
      }

      // Save city object in database
      // Beware... lots of printing
      city.save(function(err, result){
        if(err) console.log(error);
        else console.log(result);
      });
    });
  } catch(err){
    console.log(err);
  }
}

// Oh Canada
var getCanadianState = function(id){
  var state;
  switch(id){
    case "01":
      state = "AB";
      break;
    case "02":
      state = "BC";
      break;
    case "03":
      state = "MB";
      break;
    case "04":
      state = "NB";
      break;
    case "05":
      state = "NL";
      break;
    case "07":
      state = "NS";
      break;
    case "08":
      state = "ON";
      break;
    case "09":
      state = "PE";
      break;
    case "10":
      state = "QC";
      break;
    case "11":
      state = "SK";
      break;
    case "12":
      state = "YT";
      break;
    case "13":
      state = "NT";
      break;
    case "14":
      state = "NU";
      break;
    default:
      state = "LOL";
      break;
  }
  return state;
}
