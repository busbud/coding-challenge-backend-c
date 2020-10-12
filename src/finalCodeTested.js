//proceed a tsv file to json 
const fs = require("fs");
const d3 = require('d3')
let tsvData = '../data/cities_canada-usa.tsv'

let jsonData = []
fs.readFile(tsvData, 'utf-8', function(error,data) {
  //console.log(data)
  // data = d3.tsvParse(data);
  //jsonData = JSON.stringify(data);
  //fs.writeFileSync('../../data/cities_canada-usa.json', jsonData);
  //console.log("processed ..."); 
});




let json = require('../../data/cities_canada-usa.json')
let canadaCities = json.filter((el) => {
  return el.country == 'CA' 
})
//console.log(json[0])
let usCities = json.filter((el) => {
  return el.country == 'US'
})

let nonUSCACities = json.filter((el) => {
  return (el.country !== 'US' && el.country !== 'CA')
})

console.log(canadaCities.length)
console.log(usCities.length)
console.log("json size",json.length, usCities.length + canadaCities.length, nonUSCACities.length)
console.log(canadaCities[0])



///source code
https://stackoverflow.com/questions/16774935/javascript-function-nearest-neighbor


var tree = new RBush(rawData.length); 
tree.load(rawData);