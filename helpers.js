const dataFile = __dirname + '/data/cities_canada-usa.tsv';
const tsv = require('node-tsv-json');

const parseDataSource = () => {
  return new Promise((onFulfill, onReject) => {
    tsv(
      {
        input: dataFile,
        output: null,
        parseRows: false,
      },
      function (err, result) {
        if (err) {
          onReject(err);
        } else {
          onFulfill(result);
        }
      }
    );
  });
};

const findObjects = (q, data) => {
  const search = new RegExp(`.*${q}.*`, 'gm');
  const results = data.filter((s) => s.name.match(search));
  return results;
};

const sortResults = (q, data) => {
  let maxValue = 0,
    minValue = 0;
  let checkDistance = 0;

  const results = [];

  for (let i = 0; i < data.length; i++) {
    const s = data[i];
    if (q.hasOwnProperty('coordinates')) {
      checkDistance = getDistance(
        s.lat,
        s.long,
        q.coordinates.lat,
        q.coordinates.lon
      );
      if (minValue === 0) minValue = checkDistance;
      if (checkDistance > maxValue) maxValue = checkDistance;
      if (checkDistance < minValue) minValue = checkDistance;
    } else {
      checkDistance = s.name.indexOf(q);
      if (minValue === 0) minValue = checkDistance;
      if (checkDistance > maxValue) maxValue = checkDistance;
      if (checkDistance < minValue) minValue = checkDistance;
    }
    results.push({
      name: `${s.name}, ${s.admin1}, ${s.country}`,
      latitude: s.lat,
      longitude: s.long,
      distance: checkDistance,
    });
  }
  results.push({ maxValue, minValue });
  return resultsWeight(results);
};

const resultsWeight = (dataObject) => {
  const results = [];
  const { maxValue } = dataObject[dataObject.length - 1];

  dataObject.pop();
  for (let i = 0; i < dataObject.length; i++) {
    const ele = dataObject[i];
    const distancePortions = ele.distance / maxValue;
    delete ele.distance;
    results.push({
      ...ele,
      score: (1 - distancePortions).toFixed(2),
    });
  }

  return results.sort((a, b) => (a.score > b.score ? -1 : 1));
};

const getDistance = (lat1, lon1, lat2, lon2) => {
  const R = 6371; // km
  const dLat = toRad(lat2 - lat1);
  const dLon = toRad(lon2 - lon1);
  const mLat1 = toRad(lat1);
  const mLat2 = toRad(lat2);

  const a =
    Math.sin(dLat / 2) * Math.sin(dLat / 2) +
    Math.sin(dLon / 2) * Math.sin(dLon / 2) * Math.cos(mLat1) * Math.cos(mLat2);
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
  const d = R * c;
  return d;
};

const toRad = (Value) => {
  return (Value * Math.PI) / 180;
};

module.exports = {
  parseDataSource,
  findObjects,
  sortResults,
};
