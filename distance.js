/* Distance
 * ==========================
 * Calculate distance between 2 cities
 */
 var Levenshtein = require('levenshtein');
 var GeoPoint = require('geopoint');

/* @Constructor
 * @Param city_to :Object - geonames response city
 * @Param city_from :Object - city we search for
 */
 var Distance = function (city_to, city_from) {
  var that = this;
  that.from = city_from;
  that.to = city_to;


  /* @Method levenshtein :Function - calculate levenshtein distance
  */
  that.levenshtein = function () {
    var l = new Levenshtein(that.from.name, that.to.name);
    var dist = (1-l.distance/100); // to get score between 0 and 1
    return parseFloat(dist).toFixed(4);
  };

  /* @Method kilometers :Function - calculate distance in kms
  */
  that.kilometers = function () {
    var to_point = new GeoPoint(parseFloat(that.to.latitude), parseFloat(that.to.longitude));
    var from_point = new GeoPoint(parseFloat(that.from.latitude), parseFloat(that.from.longitude));

    /*calculate distance in kms between 2 cities*/
    return from_point.distanceTo(to_point, true);
  };

  /* @Method score :Function - compute score of the research
  */
  that.score = function () {
    var dist = that.levenshtein();
    /*get higher score if city is closer*/
    if (city_from.latitude != 0 && city_from.longitude != 0){
      dist = dist - that.kilometers()/100000;
    }
    return parseFloat(dist).toFixed(3);
  };
};

module.exports = Distance;