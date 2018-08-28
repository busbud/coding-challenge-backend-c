const dataLoader = require('../lib/dataLoader');

const provinceMapper = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YT',
  '13': 'NT',
  '14': 'NU',
};

function lineHandler(entry) {
  const provOrState = entry.country === 'CA' ? provinceMapper[entry.admin1] : entry.admin1;
  
  const altNameArray = entry.alt_name.length ? entry.alt_name.split(',').map(name => name.toLowerCase()) : [];
  const displayName = `${entry.name}, ${provOrState}, ${entry.country}`;
  const ascii = entry.ascii.toLowerCase();

  return {
    ...entry,
    'alt_name': altNameArray,
    ascii,
    displayName,
    population: parseInt(entry.population),
  };
}

// Load cities in a simple object in memory for this code challenge.
// In a production app these would most likely be stored in a classic database like MongoDB or PostgreSQL, possibly loaded in memory for faster access.
let cities = null;

exports.load = cb => {
  dataLoader({
    lineHandler,
    path: `${__dirname}/../data/cities_canada-usa.tsv`,
  }).then(result => {
    cities = result;
    cb();
  });
};

exports.getCities = () => cities;
