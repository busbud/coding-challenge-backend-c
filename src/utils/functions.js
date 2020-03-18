import string_similarity from 'string-similarity';

// takes in tsv formatted data and returns JSON array
export const tsvJSON = tsv => {
    const lines = tsv.split('\n');
    const headers = lines.slice(0, 1)[0].split('\t');
    return lines.slice(1, lines.length).map(line => {
        const data = line.split('\t');
        return headers.reduce((obj, nextKey, index) => {
            obj[nextKey] = data[index];
            return obj;
        }, {});
    });
};

//filters for items whose name key containds lookUpCityName and returns an array
export const getCitiesThatMatchName = (lookUpCityName, data) => {
    return data.filter(city =>
        city.name && city.name.match(lookUpCityName)
    );
};

// calculates matching score between 2 strings
export const calcNameMatchingScore = (stringA, stringB) => {
    return string_similarity.compareTwoStrings(stringA, stringB);
};

// calculates distance score between two points. returns 1 if there is no
// distance else deducts 0.05 from every 100Km of distance increment
export const calcDistanceScore = (pointA, pointB) => {
    const [x1, y1] = pointA;
    const [x2, y2] = pointB;

    //no distance = same point
    if(x1 === x2 && y1 === y2) return 1;

    const disanceInKM = distance(x1, y1, x2, y2);
    const reductionScore = (disanceInKM / 100) * 0.05;
    return Math.max(0, 1 - reductionScore);
    
};

// source https://www.geodatasource.com/developers/javascript
// calculates distance between two points
const distance = (lat1, lon1, lat2, lon2) => {
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
        //convert to KM
        dist = dist * 1.609344;
		return Math.round(dist);
	}
}

export const sortByAscendingScore = (a, b) => {
    return a.score < b.score?1:(a.score > b.score?-1:0);
};