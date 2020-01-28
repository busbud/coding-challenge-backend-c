var citiesDataSet = require('./data/cities_canada-usa.json');

function getSuggestions(params) {
  const regexp = new RegExp(params.q, 'gi');
  const minimumSearchLenght = 2;
  return params.q.length > minimumSearchLenght
    ? calculateScores(
        citiesDataSet
          .filter(function(city) {
            return (
              //First screening for name or alt_name match
              ((city.name && city.name.match(regexp)) ||
                (city.alt_name && city.alt_name.match(regexp))) &&
              // The suggestions should be restricted to cities in the USA and Canada...
              city.country &&
              city.country.match(/CA|US/) &&
              // ...with a population above 5000 people.
              city.population &&
              city.population > 5000
            );
          })
          .map(function(item) {
            // Build object using name convention. Ex: Montreal, QC, Canada
            return {
              unique_name: normalizeCityNames(
                item.name,
                item.admin1,
                item.country
              ),
              name: item.name,
              lat: item.lat,
              long: item.long,
              distance: haversine(
                params.latitude,
                params.longitude,
                item.lat,
                item.long
              )
            };
          }),
        params.q
      )
        .map(function(item) {
          return {
            name: item.unique_name,
            latitude: item.lat,
            longitude: item.long,
            score: item.score
          };
        })
        .sort(function(a, b) {
          return a.name - b.name;
        })
        .sort(function(a, b) {
          return b.score - a.score;
        })
    : [];
}
function calculateScores(data, q) {
  var distance_score = 0;
  var name_score = 0;
  const minDistance = Math.min.apply(
    Math,
    data.map(function(o) {
      return o.distance;
    })
  );
  const maxDistance = Math.max.apply(
    Math,
    data.map(function(o) {
      return o.distance;
    })
  );
  const scoreWeight = {
    exact_name_match: 1,
    partial_name_match: 0.2,
    partial_alt_name: 0.1,
    distance: 0.4
  };
  const minDistanceScore = (minDistance / maxDistance) * scoreWeight.distance;
  // Naming Scores
  return (
    data
      // Naming score
      .map(function(city) {
        // First equivalency
        var equivalency = 0;
        const minLength = Math.min.apply(null, [q.length, city.name.length]);
        const maxLength = Math.max.apply(null, [q.length, city.name.length]);
        for (var i = 0; i < minLength; i++) {
          if (q[i].toLowerCase() == city.name[i].toLowerCase()) {
            equivalency++;
          }
        }
        name_score = (equivalency / maxLength) * scoreWeight.exact_name_match;
        //Second equivalency
        if (name_score === 0) {
          name_score +=
            city.name && city.name.match(new RegExp(q, 'gi'))
              ? scoreWeight.partial_name_match
              : 0;
          name_score +=
            city.alt_name && city.alt_name.match(new RegExp(q, 'gi'))
              ? scoreWeight.partial_alt_name
              : 0;
        }

        return {
          unique_name: city.unique_name,
          distance: city.distance,
          name: city.name,
          lat: city.lat,
          long: city.long,
          score: name_score
        };
      })
      // distance Score
      .map(function(city, i, array) {
        if (!city.distance) {
          distance_score = 0;
          return city;
        }

        distance_score =
          (city.distance / maxDistance) * scoreWeight.distance -
          minDistanceScore;

        return {
          unique_name: city.unique_name,
          name: city.name,
          lat: city.lat,
          long: city.long,
          score:
            city.score - distance_score < 0.2
              ? 0.2
              : (city.score - distance_score).toFixed(1)
        };
      })
  );
}
function haversine(lat1, lon1, lat2, lon2) {
  var p = Math.PI / 180;
  var c = Math.cos;
  var a =
    0.5 -
    c((lat2 - lat1) * p) / 2 +
    (c(lat1 * p) * c(lat2 * p) * (1 - c((lon2 - lon1) * p))) / 2;

  return 12742 * Math.asin(Math.sqrt(a));
}
function normalizeCityNames(name, admin1, country) {
  const canadaProvinces = [
    { code: '01', init: 'AB', name: 'Alberta' },
    { code: '02', init: 'BC', name: 'British Columbia' },
    { code: '03', init: 'MB', name: 'Manitoba' },
    { code: '04', init: 'NB', name: 'New Brunswick' },
    { code: '05', init: 'NL', name: 'Newfoundland and Labrador' },
    { code: '07', init: 'NS', name: 'Nova Scotia' },
    { code: '08', init: 'ON', name: 'Ontario' },
    { code: '09', init: 'PE', name: 'Prince Edward Island' },
    { code: '10', init: 'QC', name: 'Quebec' },
    { code: '11', init: 'SK', name: 'Saskatchewan' },
    { code: '12', init: 'YT', name: 'Yukon' },
    { code: '13', init: 'NT', name: 'Northwest Territories' },
    { code: '14', init: 'NU', name: 'Nunavut' }
  ];
  const item = {
    name: name || '',
    area: admin1 || '',
    country: country || ''
  };

  if (country == 'CA') {
    item.country = 'Canada';

    item.area =
      canadaProvinces.filter(function(element) {
        if (element.code && admin1) {
          return element.code == admin1;
        } else {
          return false;
        }
      })[0].init || '';
  } else if (country == 'US') {
    item.country = 'United States';
    item.area = admin1;
  } else {
    item.area = admin1;
  }
  return [item.name, item.area, item.country].join(', ');
}
exports.getSuggestions = getSuggestions;
