// @class city
// This class holds the city information and the logic for calculating  the 
// the distance with a reference latitude and longitude.

var logger = require('../helpers/logger'),
__bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };

var PROVINCES={"01":"AB","02":"BC","03":"MB","04":"NB","05":"NL","07":"NS","13":"NT","14":"NU","08":"ON","09":"PE","10":"QC","11":"SK","12":"YT"};
var COUNTRIES={"US":"USA","CA":"Canada"};

module.exports = (function() {
  'use strict';
  
  // @constructor
  // @param options
  // @param {string} name
  // @param {string} country
  // @param {number} population
  // @param {number} lat
  // @param {number} long
  function City(name, ascii, province, country, population, lat, long) {
    this.log = logger('model:city');

    this.name = this.set('name', name);
    this.ascii = this.set('ascii', ascii);

    if(typeof(COUNTRIES[country.toUpperCase()]) != 'undefined') this.country = this.set('country', COUNTRIES[country.toUpperCase()]);
    if(typeof(PROVINCES[province.toUpperCase()]) != 'undefined' && this.country === 'Canada') {
      this.province = this.set('province', PROVINCES[province.toUpperCase()]);
    } else {
      this.province = this.set('province', province.toUpperCase());
    }

    this.population = this.set('population', population);
    this.lat = this.set('lat', lat);
    this.long = this.set('long', long);

    this.fields = __bind(this.fields, this);
    this.set = __bind(this.set, this);

  }

  // Map of Fields with type.
  City.prototype.fields = {
      'name':'string',
      'ascii':'string',
      'country':'string',
      'province':'string',
      'population':'number',
      'lat':'number',
      'long':'number'     
  }


  // Set value of field.
  // @param {string} name - name of field
  // @param {string} or {number} value - value of field
  City.prototype.set = function(name, value) {
    if(typeof(this.fields[name]) === 'undefined'){
      this.log('%s is not a city field', name);
    } else {
      if(this.fields[name] === typeof(value)) {
        return value;
      } else if(typeof(value) === 'string') {
        if(this.fields[name] === 'number'){
          return parseFloat(value);
        }
      } else if(typeof(value) === 'number') {
        if(this.fields[name] === 'string'){
          return ''+value;
        }
      }
    }
  }

  // Computes distance with a reference latitude and longitude.
  // @param {number} refLat in degrees
  // @param {number} refLong in degrees
  // base on http://www.movable-type.co.uk/scripts/latlong.html
  City.prototype.distance = function(refLat, refLong) { 
    //Converts degrees to radians.
    var toRadians = function(a) {
        return a * Math.PI / 180;
    }

    var earthRadius = 6371; // km
    var refLatRad = toRadians(refLat);
    var latRad = toRadians(this.lat);
    var deltaLatRad = toRadians(this.lat - refLat);
    var deltaLonRad = toRadians(this.long - refLong);

    var a = Math.sin(deltaLatRad/2) * Math.sin(deltaLatRad/2) +
            Math.cos(refLatRad) * Math.cos(latRad) *
            Math.sin(deltaLonRad/2) * Math.sin(deltaLonRad/2);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));

    return earthRadius * c;
  };


  return City;
})();


