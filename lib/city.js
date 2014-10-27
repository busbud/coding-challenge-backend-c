var _ = require('lodash');

var CITY_ALLOWED_PARAMS = [
  'name',
  'ascii',
  'admin1',
  'country',
  'lat',
  'long'
];
var COUNTRIES = {
  'CA': 'Canada',
  'US': 'USA'
};
var CANADIAN_PROVINCES = {
  1:  'AB',
  2:  'BC',
  3:  'MB',
  4:  'NB',
  5:  'NL',
  7:  'NS',
  8:  'ON',
  9:  'PE',
  10: 'QC',
  11: 'SK',
  12: 'YT',
  13: 'NT',
  14: 'NU'
};

function City(params) {
  var params = _.pick(params, CITY_ALLOWED_PARAMS);

  // Set admin1 for canada to 2 letters
  // Original dataset had numbers for canadian provinces
  if (params.country === 'CA') {
    params.admin1 = CANADIAN_PROVINCES[parseInt(params.admin1)];
  }

  // Set country to fullname
  params.country = COUNTRIES[params.country];

  // Assign selected params to ourselves
  _.assign(this, params);
}

City.prototype.toObject = function() {
  return {
    name: [
        this.ascii,
        this.admin1,
        this.country
      ].join(', '),
    latitude: this.lat,
    longitude: this.long,
    score: this.score
  };
};

module.exports = City;
