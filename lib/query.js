var GeoPoint = require('./geo-point');

function Query(data) {

  if (!data.q && data.q.length >= 2) {
    throw new Error('q parameter is required and should contains at least 2 characters');
  }
  this.q = data.q;
  if (data.latitude || data.longitude) {
    this.geoPoint = new GeoPoint(data.latitude || data.longitude);
  }
}

module.exports = Query;