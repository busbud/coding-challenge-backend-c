const city_parser = require("./cities_tsv_parser");

/**
 * 
 * @param {string} query The complete or partial search term for suggestions.
 * @param {number} latitude The latitude of the caller's location. Helps with score's accuracy.
 * @param {number} longitude The longitude of the caller's location. Helps with score's accuracy.
 * @param {number} max_amount Limits the amount of results the endpoint returns.
 * @param {boolean} loose_match If "true", tries to match results, even when there are small typos. But reduces performance and score's accuracy.
 * @returns {Promise<Array>}
 */
const searchDataSet = async (query, latitude, longitude, max_amount = 7, loose_match = "true") => {
    if(!query) return [];

    const cities = await city_parser.getCities();
    
    const normalized_query = query.toLowerCase().normalize("NFD").replace(/\p{Diacritic}/gu, ""); 
    const regx = new RegExp(normalized_query);

    const filtered = cities.filter((city, idx) => {
        if(city) {

            // Removes "(" and ")". A more complete solutions would remove all special symbols.
            const normalized_city_name = city.asciiname.toLowerCase().replace(/\(\)/,'');

            if(normalized_city_name === normalized_query) {
                city.score = 1;
                return true;
            } else if(normalized_city_name.match(regx)) {
                city.score = 0.8;
                return true;
            } else if(loose_match === "true" && findLoose(normalized_query, normalized_city_name)) {
                city.score = 0.5;
                return true;
            }
        }

        return false;
    });

    const result = filtered.map((city, idx) => {
        let name = `${city.name}, ${city.admin1_code}, ${city.country_code == "US"? "USA": "Canada"}`;
        return {name: name, latitude: city.latitude, longitude: city.longitude, score: city.score};
    });

    if(latitude && longitude) {

        result.forEach((city, idx) => {
            if(latitude == city.latitude && longitude == city.longitude){
                city.score = 1;
                return;
            }

            const score = findDistanceAndScore(latitude, city.latitude, longitude, city.longitude);
            
            city.score = score;

        });

    }

    return result.sort((a, b) => a.score > b.score? -1: 1).slice(0, max_amount);
    
};

/**
 * Uses "Spherical Law of Cosines" to find the distances between 2 points and return an score.
 * 
 * @param {number} lat1 
 * @param {number} lat2 
 * @param {number} lon1 
 * @param {number} lon2 
 * @returns {number}
 */
const findDistanceAndScore = (lat1, lat2, lon1, lon2) => {
    const phi_1 = lat1 * Math.PI/180;
    const phi_2 = lat2 * Math.PI/180;

    const delta_lambda = (lon2-lon1) * Math.PI/180; 
    const radius = 6371e3;

    const distance = Math.acos( Math.sin(phi_1)*Math.sin(phi_2) + Math.cos(phi_1)*Math.cos(phi_2) * Math.cos(delta_lambda) ) * radius;

    let score = 0;

    if(distance < 150000) score = 0.9;
    else if(distance < 250000) score = 0.8;
    else if(distance < 350000) score = 0.7;
    else if(distance < 450000) score = 0.6;
    else if(distance < 550000) score = 0.5;
    else if(distance < 650000) score = 0.4;
    else if(distance < 750000) score = 0.3;
    else score = 0.2;
    
    return score;
}

const findLoose = (query, source) => {
    var regx = new RegExp(query.split('').map((ele, idx) => { 
        return query.substr(0, idx) + ele + '?.?' + query.substr(idx+1);
    }).join('|'),'gi');

    return source.match(regx) != null;
}

exports.searchDataSet = searchDataSet;