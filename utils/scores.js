levenshteinDistanceScore = require("./levenshteinDistance.js");

/*
    Well, this is improvised. Cause I had no information about it ^_^
*/
const getScore = (item, params) => {
  const wordScore = levenshteinDistanceScore(item.name, params.filterWord);
  if (!!params.latitude && !!params.longitude) {
    /*
        The first thing came up to my mind was there are 180 latitudes and 360 longitudes.
        So I thought to take the coordinate system as a rectangle for the simplicity (I know it's not ^_^),
        in this coordinate system If an exact coordinate is 1, I can just lower the score by calculating how far it is
        from the coordinate I have. Since there are 180 latitudes and 360 longitudes, I simply pretended the calculation
        is just like as follows.
    */
    // const latitudeScore = 1 - Math.abs(item.latitude - params.latitude) / 180;
    // const longitudeScore = 1 - Math.abs(item.longitude - params.longitude) / 360;

    /*
        After a couple test I decided to change it as follows. So basically if it's far enough I just calculate the score 0.
    */
    let latitudeScore = 1 - Math.abs(item.latitude - params.latitude) / 9;
    let longitudeScore = 1 - Math.abs(item.longitude - params.longitude) / 18;
    latitudeScore = latitudeScore > 0 ? latitudeScore : 0;
    longitudeScore = longitudeScore > 0 ? longitudeScore : 0;
    // I wasn't so sure about how to distribute the effect to the score.
    // So all the parameters have the same effect basically
    return wordScore / 3 + latitudeScore / 3 + longitudeScore / 3;
  } else {
    return wordScore;
  }
};

const getDataWithScores = (filteredData, params) => {
  return filteredData
    .map((item) => {
      return { ...item, score: getScore(item, params) };
    })
    //Sorting by the scores
    .sort((a, b) => b.score - a.score)
    .filter((item) => item.score >= 0.3)
    .map((item) => {
      return {
        name: item.name + ", " + item.admin1 + ", " + item.country,
        latitude: item.latitude,
        longitude: item.longitude,
        //used Math.floor to fix the floating point to 1 however
        //I would like to know if there's a better way to do that
        score: Math.floor(item.score * 10) / 10,
      };
    });
};

module.exports = getDataWithScores;
