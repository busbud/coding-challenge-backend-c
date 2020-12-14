var express = require('express');
var router = express.Router()
var fs = require('fs')
var Diacritics = require('diacritic');

// Reading the data
let data = fs.readFileSync("./data/cities_canada-usa.tsv", "utf-8")
const json = data.split('\n').map(city => {
  const p = city.split('\t');
  return {
    id: p[0],
    name: p[1],
    ascii: p[2],
    alt_name: p[3],
    lat: p[4],
    long: p[5],
    feat_class: p[6],
    feat_code: p[7],
    country: p[8],
    cc2: p[9],
    admin1: p[10],
    admin2: p[11],
    admin3: p[12],
    admin4: p[13],
    population: p[14],
    elevation: p[15],
    dem: p[16],
    tz: p[17],
    modified_at: p[18]
  };
});
// console.log(json)

router.get('/', function(req, res){
    try {
        const ID = req.query.q;
        const lat = req.query.latitude;
        const long = req.query.longitude;
        let score;
        let suggestions = []
        json.filter((elem)=> {
            if(elem.name && Diacritics.clean(elem.name.toLowerCase()).includes(Diacritics.clean(ID.toLowerCase())) && elem.population && elem.population > 5000) {
                if(long && lat) {
                    let dist = distance(elem.lat, elem.long, lat, long)
                        let name = `${elem.name}, ${elem.admin1}, ${elem.country}`;
                        suggestions.push({
                            "name": Diacritics.clean(name),
                            "latitude": elem.lat,
                            "longitude": elem.long,
                            "score": (1 - (dist/1000).toFixed(1))
                        })
                    
                } else {
                    let name = `${elem.name}, ${elem.admin1}, ${elem.country}`;
                        
                    suggestions.push({
                        "name": Diacritics.clean(name),
                        "latitude": elem.lat,
                        "longitude": elem.long,
                        "score": 0.9
                    }) 
                }
            }
                
        })
        if(suggestions.length > 1)
            suggestions.sort((a,b) =>  a.score >  b.score ? -1 : 1)
        if(suggestions.length == 0) {
            res.writeHead(404, {'Content-Type': 'text/plain'});
        } else {
            res.writeHead(200, {'Content-Type': 'application/json'});
        }
        res.end(JSON.stringify({suggestions: suggestions}))
    } catch(err){
        console.log("Error while retreiving data")
        res.writeHead(404, {'Content-Type': 'text/plain'});
        res.end(JSON.stringify({suggestions: []}))
    }
})

function distance(lat1, lon1, lat2, lon2, unit) {
	if ((lat1 == lat2) && (lon1 == lon2)) {
		return 0;
	}
	else {
		var radlat1 = Math.PI * lat1/180;
		var radlat2 = Math.PI * lat2/180;
		var theta = lon1-lon2;
		var radtheta = Math.PI * theta/180;
		var dist = Math.sin(radlat1) * Math.sin(radlat2) + Math.cos(radlat1) * Math.cos(radlat2) * Math.cos(radtheta);
		if (dist > 1) {
			dist = 1;
		}
		dist = Math.acos(dist);
		dist = dist * 180/Math.PI;
		dist = dist * 60 * 1.1515;
		
		return dist;
	}
}



module.exports = router;
