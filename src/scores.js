const levenshtein = function (a, b) {

  a = a.toLowerCase();
  b = b.toLowerCase();

  if (a === b) {
    return 0;
  }

  if (a.length === 0) return b.length;
  if (b.length === 0) return a.length;

  var v0 = [];
  var v1 = [];

  for (var i = 0; i < b.length + 1; i++) {
    v0[i] = i;
  }

  for (var i = 0; i < a.length; i++) {
    v1[0] = i + 1;
    for (var j = 0; j < b.length; j++) {
      var cost = a[i] === b[j] ? 0 : 1;
      v1[j + 1] = Math.min(v1[j] + 1, v0[j + 1] + 1, v0[j + cost]);
    }

    for (var j = 0; j < v0.length; j++) {
      v0[j] = v1[j];
    }
  }

  return v1[b.length];

};


// Calculate distance with haversine
const proximity = function (a, b) {

  if (!a.lat || !a.lng || !b.lat || !b.lng) {
    return null;
  } else {
    a = {
      lat: parseFloat(a.lat),
      lng: parseFloat(a.lng)
    };
    b = {
      lat: parseFloat(b.lat),
      lng: parseFloat(b.lng)
    };
  }

  const R = 6371;

  const dLat = (b.lat - a.lat).toRad();
  const dLng = (b.lng - a.lng).toRad();

  const arc = Math.pow(Math.sin(dLat / 2), 2) +
    Math.cos(b.lat.toRad()) * Math.cos(a.lat.toRad()) *
    Math.pow(Math.sin(dLng / 2), 2);

  return R * 2 * Math.atan2(Math.sqrt(arc), Math.sqrt(1 - arc));
};


Number.prototype.toRad = function () {
  return this * Math.PI / 180;
};


module.exports = {
  levenshtein: levenshtein,
  proximity: proximity
};