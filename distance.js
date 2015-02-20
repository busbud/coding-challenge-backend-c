/* Distance
 * ==========================
 * Calculate distance between 2 cities
 */
 var Levenshtein = require('levenshtein');
 var GeoPoint = require('geopoint');

/* @Constructor
 * @Param cityTo :Object - geonames response city
 * @Param cityFrom :Object - city we search for
 */
 var Distance = function (cityTo, cityFrom) {
  var that = this;
  that.from = cityFrom;
  that.to = cityTo;


  /* @Method levenshtein :Function - calculate levenshtein distance
  */
  that.levenshtein = function () {
    var l = new Levenshtein(that.from.name, that.to.name);
    var dist = (1-l.distance/10); // to get score between 0 and 1
    return (dist < 0 ? 0 : parseFloat(dist).toFixed(4));
  };

  /* @Method kilometers :Function - calculate distance in kms
  */
  that.kilometers = function () {
    var toPoint = new GeoPoint(parseFloat(that.to.latitude), parseFloat(that.to.longitude));
    var fromPoint = new GeoPoint(parseFloat(that.from.latitude), parseFloat(that.from.longitude));

    /*calculate distance in kms between 2 cities*/
    return fromPoint.distanceTo(toPoint, true);
  };

  /* @Method score :Function - compute score of the research
  */
  that.score = function () {
    var dist = that.levenshtein();
    /*get higher score if city is closer*/
    if (cityFrom.latitude != 0 && cityFrom.longitude != 0){
      var kms = that.kilometers();
      dist = (kms > 100000 ? 0 : (dist - kms/100000));
    }
    return parseFloat(dist).toFixed(2);
  };
};

module.exports = Distance;