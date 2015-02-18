/* Geoapi
 * ==========================
 * Wrapper for http://www.geonames.org/ api
 */
 var city = require('./city');
 var distance = require('./distance');
 var _ = require('underscore-node');
 var request = require('request');

/* @Constructor
 * @Param username :String - username to geonames.org
 */
 var Geoapi = function (username) {
  /*Geonames webservice parameters*/
  var KEYS = ['username', 'q', 'name', 'name_equals', 'name_startsWith',
  'maxRows', 'startRow', 'country', 'countryBias', 'adminCode1', 'adminCode2',
  'adminCode3','featureClass','featureCode','cities','lang','type',
  'style', 'isNameRequired', 'tag', 'operator', 'charset', 'fuzzy',
  'east', 'west', 'north', 'south', 'searchlang', 'orderby'];

  var that = this;

  that.username = username;

  that.filter = { cities: "cities5000",
    username: that.username,
    fuzzy: 0.5,
    type: "json",
    country: "CA,US"
  }

  /* @Method cities :Function - transform geonames response to city object list
   * @Param response :Object - response from geonames request (search method)
   * @Param citySearch :Object - searched city object
   */
   that.cities = function (response, citySearch) {
    return _.sortBy(_.map(response.geonames, function(mapCity) {
      var responseCity = new city(mapCity.name);
      responseCity.latitude = mapCity.lat;
      responseCity.longitude =  mapCity.lng;
      responseCity.region = mapCity.adminName1;
      responseCity.country = mapCity.countryName;
      var dist = new distance(responseCity,citySearch);
      responseCity.score = parseFloat(dist.score());
      responseCity.longName();
      return _.pick(responseCity, 'name', 'latitude', 'longitude', 'score');
    }),'score').reverse();
  };

  /* @Method citySearched :Function - create object for searched city
   * @Param query :Object - query from the browser url
   */
  that.citySearched = function (query) {
    /*city object we search for*/
    var citySearch = new city(query.q);

    if (query.latitude != undefined && query.longitude != undefined){
      citySearch.latitude = query.latitude;
      citySearch.longitude = query.longitude;
    }
    return citySearch;
  };

  /* @Method search :Function - sends out request to geonames server
   * @Param query :Object - query from the browser url
   * @Param callback :Function - Function to pass error data back to
   */
   that.search = function (query, callback) {

    var params = _.extend(query, that.filter);//merge url params and config params
    params = _.pick(params, KEYS);//keep only geonames compatible params

    /*request send to geonames*/
    request.get({
      url : "http://api.geonames.org/search?",
      qs : params
    }, function (err, res, body) {
      if (! err) {
        //transform geonames response to city object list
        callback( null, that.cities(JSON.parse(body),that.citySearched(query)));
      }else{
        callback( null, err);
      }

    });

  };
};

module.exports = Geoapi;