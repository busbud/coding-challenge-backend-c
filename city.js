/* City
*/
var City = function (name) {
  var that = this;
  that.name = name;
  that.region = null;
  that.country = null;
  that.latitude = 0;
  that.longitude = 0;
  that.score = 0;
  that.longName = function () {
    that.name = that.name + ', ' + that.region + ', ' + that.country;
    return that.name + ', ' + that.region + ', ' + that.country;
  };
};

module.exports = City;