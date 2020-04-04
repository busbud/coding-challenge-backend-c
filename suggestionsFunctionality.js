const fs = require("fs");

module.exports = function autocompleteSuggestions(input) {
    //parse url 
    var params = urlParser(input);
    if(params === undefined || params.length == 0) {
        return;
    } else {     
        return suggestions(params);;
    }
}
/**
 * Funtion will call helper functions in order to create
 * an array of suggestions to return to the client.
 * 
 * @param {Array} params containing query parameters
 * @returns Array - with any/all possible suggestions.
 */
function suggestions(params) {
    // read in the file and convert it into a json obj
    tsvData = tsvparser("data/cities_canada-usa.tsv");
    // First check spelling and match to possible cities
    var sugg = froontbackSearch(params[0], tsvData);      
    // if lat and long is given continue filtering possible suggestions
    if(params[1] && params[2]){
        /* Pass the current suggestions to the 
        function and the absolute value of the of long and lat. */
        sugg = latlongScore(sugg, 
            Math.abs(parseFloat(params[1]) + parseFloat(params[2])));
    }

    return sugg;
}
/**
 * Function will compare the lat and long of each suggestion 
 * and will deduct a point per distance, the absolute value 
 * of the lat and long is used to create a center point.
 * 
 * @param {Array} sugg - Array of suggestions
 * @param {Float} latlong - Absolute value of the sum of the lat and long
 * @returns Array - containing possible suggestions.
 */
function latlongScore(sugg, latlong) {   
    for (let i = 0; i < sugg.length; i++) {        
        /* The distance between the suggested lat and long was added
            up and so was the lat and long from the input the client made,
            if the absolute value of subtracting the two is zero then it
            is the same location. */
        var dis = Math.abs((sugg[i].distance - latlong));
        /* If it's not the exact lat and long deduct points per distance 
           from location given by client. A cap of 0.5 is set inorder to not
           deduct to many points */
        if((sugg[i].distance - latlong) != 0) { 
            sugg[i].score = (dis / 100) >= 0.5 ? sugg[i].score - 0.5 :
                                 (Math.abs(sugg[i].score - (dis / 100))).toFixed(2);  
        } else if((sugg[i].distance - latlong) == 0 && sugg[i].score == 1) {
            /* If score is 1 the city name was an excat match and if the lat and long is 0
                the lat and long given was also a perfect match.
            */
            return [sugg[i]];
        }         
    }   
    return sugg;
}
/**
 * Function will return an array containing the suggestions
 * for the client, it will take the query and itterate through
 * the data in order to match the possible matches.
 * 
 * @param {String} q - query city entered on the url.
 * @param {Array} searchArr  - content of the TSV file.
 * @returns Array - containing possible suggestions.
 */
function froontbackSearch(q, searchArr) {
    var suggestions = [];
    var front = 0;
    var back =  searchArr.length - 1;
    // iterate through the data, comparing the first and las items at the same time
    while(front <= back) {  
        /* compare only x number of charecters given the q length and assign
            and assign it to a variable if it is defined */        
        qLowerCase = q.toLowerCase();
        frontLetter = compareName(searchArr[front], qLowerCase);
        backLetter = compareName(searchArr[back], qLowerCase);         
        if(frontLetter.cityName == qLowerCase && frontLetter.cityPopulation >= 5000) {   
            suggestions.push({
                id: searchArr[front].id,
                name: frontLetter.cityFullName,
                latitude: searchArr[front].lat,
                longitude: searchArr[front].long,
                distance:  Math.abs(parseFloat(searchArr[front].lat) + parseFloat(searchArr[front].long)),
                country: searchArr[front].country,
                sp: searchArr[front].tz.split('/')[1],
                score: spellCheckHellper(q, frontLetter.cityFullName)
            });       
        } else if (backLetter.cityName == qLowerCase && backLetter.cityPopulation >= 5000) {
            suggestions.push({
                id: searchArr[back].id,
                name: backLetter.cityFullName,
                latitude: searchArr[back].lat,
                longitude: searchArr[back].long,
                distance:  Math.abs(parseFloat(searchArr[back].lat) + parseFloat(searchArr[back].long)),
                country: searchArr[back].country,
                sp: searchArr[back].tz.split('/')[1],
                score: spellCheckHellper(q, backLetter.cityFullName)
            });
        } else if (front == back && frontLetter.cityName == qLowerCase && frontLetter.cityPopulation >= 5000) {
            suggestions.push({
                id: searchArr[front].id,
                name: frontLetter.cityFullName,
                latitude: searchArr[front].lat,
                longitude: searchArr[front].long,
                distance:  Math.abs(parseFloat(searchArr[front].lat) + parseFloat(searchArr[front].long)),
                country: searchArr[front].country,
                sp: searchArr[front].tz.split('/')[1],
                score: spellCheckHellper(q, frontLetter.cityFullName)
            });
        }
        front++;
        back--;        
    }
    return suggestions;
}
/**
 * Helper function that verifies if the the query entered by the client
 * matches either the name, alt_name or ascii name of the city found
 * on the tsv file. Returns and object containg an abbreviated version of the
 * city name, the full original name and aswell as the population of said city
 * @param {Object} city - contains all information of a city
 * @param {String} qLowerCase - lowercase of city input from url 'q'
 * @returns Object - with the city name, population and full nam of the city
 */
function compareName(city, qLowerCase) {
    // Assgin city name if exists, but only for the the range of query given
    cityName = city.name ? city.name.substring(0, qLowerCase.length).toLowerCase() : '';
    // Assgin city alt name if exists, but only for the the range of query given
    cityAltName = city.alt_name ? city.alt_name.substring(0, qLowerCase.length).toLowerCase() : '';
    // Assgin city ascii name if exists, but only for the the range of query given
    cityAsciiName = city.ascii ? city.ascii.substring(0, qLowerCase.length).toLowerCase() : '';
    // Keep track of the full city name inorder to add it to resultset
    cityFullName = city.name ? city.name : '';
    // Get population size for the city.
    cityPopulation = parseFloat(city.population);

    // determine if the name matches to the alt_name or ascii name of the city.
    if(qLowerCase == cityAltName) {
        cityName = cityAltName;
        cityFullName = city.alt_name ? city.alt_name : '';
    } else if(qLowerCase == cityAsciiName) {
        cityName = cityAsciiName;
        cityFullName = city.ascii ? city.ascii : '';
    }
    
    return {cityName: cityName, cityPopulation: cityPopulation, cityFullName: cityFullName};

}

/**
 * Helper function that iterates throgh each charecter of the 
 * given query and verifies if the capatalization is correct,
 * if it is not a 0.1 point will be deducted if lat long is given
 * a max of 0.5 points can be deducted from the over all score.  
 *
 * @param {String} q 
 * @param {String} cityFullName 
 * @returns Float - The score the query has recived
 */
function spellCheckHellper(q, cityFullName) {
    // If original query does not match the data from the file deduc a point
    var score =  q.length == cityFullName.length ? 1.0 : 0.9;
    for (let i = 0; i < cityFullName.length; i++) {
        if(q[i] != cityFullName[i]) {            
            score -= 0.1;
        }
    }    
    return score < 0.5 ? 0.5 : score.toFixed(2); 
}

/**
 * Reads in the the file and returns a json object
 * containing the data of said file.
 * 
 * @param {String} file - file path
 * @returns Object - containg file content
 */
function tsvparser(file) {
    return tsvJSON(fs.readFileSync(file, 'utf8'));   
}
/**
 * Helpper function to parse tsv file into a json object. 
 * by: lamuertepeluda on github,
 * date: April 16 2019
 * Available: https://gist.github.com/iwek/7154706
 * 
 * @param {String} tsv 
 */
function tsvJSON(tsv) {
    var lines = tsv.split('\n');
    var headers = lines.slice(0, 1)[0].split('\t');
    return lines.slice(1, lines.length).map(line => {
      var data = line.split('\t');
      return headers.reduce((obj, nextKey, index) => {
        obj[nextKey] = data[index];
        return obj;
      }, {});
    });
  }
/**
 * Parses the the url returning an array with the given
 * information if any else returns an empty array.
 * @param {string} input 
 * @return Array - containing the parameters data if any.
 */
function urlParser(input) {
    if(input.indexOf('?') == -1){
        //no params given return empty array
        return [];        
    }
    // use string manipulation to get params from url
    var params = input.substr(input.indexOf('?') + 1).split('&');
    for (i = 0; i < params.length; i++) {
        params[i] = params[i].substr(params[i].indexOf('=') + 1);
    }    
    return params;
}