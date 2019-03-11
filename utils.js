
let MAX_DIST = 84;
let MIN_DIST = 0;


async function updateMinAndMaxDist(ConstantModel){
    try{
        // console.log("Attempted to update the max and min constants in the system for distance calculations");
        const minValue = await ConstantModel.findOne({name: 'minDistance'}).exec();
        const maxValue = await ConstantModel.findOne({name: 'maxDistance'}).exec();
        // console.log(`min: ${minValue} max: ${maxValue}`);
        if (minValue && minValue.value) MIN_DIST = minValue.value;
        if (maxValue && maxValue.value) MAX_DIST = maxValue.value;
        console.log("Successfully updated the min and max constants");
    }catch (e) {
        console.log("Failed to update max and min distance values");
    }
}


function transformForClient(city){
    return {
        "name": city.display_name,
        "asciiname": city.asciiname,
        "latitude": city.latitude,
        "longitude": city.longitude,
        "score": 1
    };
}

function transformCitiesForClient(city){
    return {
        "name": city.display_name,
        "asciiname": city.asciiname,
        "latitude": city.latitude,
        "longitude": city.longitude,
        "country_code": city.country_code
    }
}


// https://en.wikipedia.org/wiki/Levenshtein_distance
// Using the Levenshtein as a reasonable interpretation of string similarity

function levenshteinDistance(string1, string2) {
    // Create empty edit distance matrix for all possible modifications of
    // substrings of string1 to substrings of string2.
    const distanceMatrix = Array(string2.length + 1).fill(null).map(() => Array(string1.length + 1).fill(null));

    // Fill the first row of the matrix.
    // If this is first row then we're transforming empty string to string1.
    // In this case the number of transformations equals to size of string1 substring.
    for (let i = 0; i <= string1.length; i += 1) {
        distanceMatrix[0][i] = i;
    }

    // Fill the first column of the matrix.
    // If this is first column then we're transforming empty string to string2.
    // In this case the number of transformations equals to size of string2 substring.
    for (let j = 0; j <= string2.length; j += 1) {
        distanceMatrix[j][0] = j;
    }

    for (let j = 1; j <= string2.length; j += 1) {
        for (let i = 1; i <= string1.length; i += 1) {
            const indicator = string1[i - 1] === string2[j - 1] ? 0 : 1;
            distanceMatrix[j][i] = Math.min(
                distanceMatrix[j][i - 1] + 1, // deletion
                distanceMatrix[j - 1][i] + 1, // insertion
                distanceMatrix[j - 1][i - 1] + indicator, // substitution
            );
        }
    }

    return distanceMatrix[string2.length][string1.length];
}

function normalizeStringDistance(diff, string1, string2){
    const max = Math.max(string1.length, string2.length);
    const min = 0;

    return Math.abs((diff - min) / (max - min));
}

// Normalise distance between 1 and 0 using feature scaling
function normalizeDistance(diff) {
    if (diff < 0 ) diff *= -1;
    console.log(`Using the following values for normalisation => min: ${MIN_DIST} max: ${MAX_DIST}`);
    return Math.abs((diff - MIN_DIST) / (MAX_DIST - MIN_DIST));
}

// euclidean distance SQRT( (Y1 - Y2)^2 + (X1 - X2) ^ 2 ) / can use Spherical Law of Cosines, but Euclidean distance easier to computer
function getDistance(city1Lat, city1Long, city2Lat, city2Long){
    return Math.sqrt(Math.pow( city1Lat - city2Lat, 2) + Math.pow( city1Long - city2Long ,2));
}


function generateScore(cityRec, cityNameQuery, latitude, longitude){

    const distWeight = 3;
    const strWeight = 1;

    let strDiff = levenshteinDistance(cityRec.asciiname, cityNameQuery);
    strDiff = normalizeStringDistance(strDiff, cityRec.asciiname, cityNameQuery);
    console.log(`${cityRec.asciiname} : (query) = (${cityNameQuery}) : diff = ${strDiff.toFixed(2)}`);
    cityRec.score -= (strDiff * strWeight).toFixed(2);

    if (latitude !== -1 && longitude !== -1){
        let locationDiff = getDistance(cityRec.latitude, cityRec.longitude, latitude, longitude);
        locationDiff = normalizeDistance(locationDiff);
        console.log(`${cityRec.name} : (x,y) = (${cityRec.longitude}, ${cityRec.latitude}) : diff = ${locationDiff.toFixed(2)}`);
        cityRec.score -= (locationDiff * distWeight).toFixed(2);
    }

    // cityRec.score = cityRec.score.toFixed(2);
    delete cityRec['asciiname'];

    return cityRec;
}


function sortByScore(cityA, cityB){
    return cityB.score - cityA.score;
}


module.exports = {
    'transformForClient': transformForClient,
    'transformCitiesForClient': transformCitiesForClient,
    'normalizeDistance': normalizeDistance,
    'generateScore': generateScore,
    'sortByScore': sortByScore,
    'getDistance': getDistance,
    'updateMinAndMaxDist': updateMinAndMaxDist,
    'MAX_DIST': MAX_DIST,
    'MIN_DIST': MIN_DIST
};

console.log(levenshteinDistance("landan", "london"));