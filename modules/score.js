require("string_score");
const geodist = require("geodist");

module.exports = (() => {
    /*
    * calculate score from city name and asked label
    * */
    const scoreStringsByName = stringToMatch => {
        return objArray => {
            return new Promise((resolve, reject) => {
                try {
                    objArray.forEach((element) => {
                        element.score = element.name.score(stringToMatch);
                    });
                    resolve(objArray);
                } catch (ex) {
                    reject(ex);
                }
            });
        };
    };

    /*
    * Add distance between cities and asked lat/long
    * */
    const setDistance = (latitude, longitude) => {
        return objArray => {
            return new Promise((resolve, reject) => {
                try {
                    if (!latitude || !longitude) {
                        resolve(objArray);
                        return;
                    }
                    objArray.forEach(element => {
                        element.dist = geodist({lat: latitude, lon: longitude}, {
                            lat: element.latitude,
                            lon: element.longitude
                        });
                    });
                    resolve(objArray);
                } catch (ex) {
                    reject(ex);
                }
            });
        };
    };

    /*
    * Returns min and max distance from suggestion array
    * */
    const getMinMaxDistance = objArray => {
        const minMaxDist = {
            min: objArray[0].dist,
            max: objArray[0].dist
        };
        objArray.forEach(element => {
            if (element.dist < minMaxDist.min) {
                minMaxDist.min = element.dist;
            } else if (element.dist > minMaxDist.max) {
                minMaxDist.max = element.dist;
            }
        });
        return minMaxDist;
    };

    /*
    * calculate score from name and distance
    * */
    const scoreFromNameAndDistance = objArray => {
        return new Promise((resolve, reject) => {
            try {
                if (objArray.length > 0 && objArray[0].dist !== undefined) {
                    let scoreDist;
                    //dist has been calculated
                    const minMaxDist = getMinMaxDistance(objArray);
                    const maxMinGap = minMaxDist.max - minMaxDist.min;
                    objArray.forEach(element => {
                        scoreDist = 1 - (element.dist - minMaxDist.min) / maxMinGap;
                        //mean score between name and dist
                        element.score = (element.score + scoreDist) / 2;
                        //remove extra property
                        delete element.dist;
                    });
                }
                resolve(objArray);
            } catch (ex) {
                reject(ex);
            }
        });
    };

    return {
        scoreStringsByName: scoreStringsByName,
        setDistance: setDistance,
        scoreFromNameAndDistance: scoreFromNameAndDistance
    };
})();
