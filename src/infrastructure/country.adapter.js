const buildUSCountry = () => ({
  code: 'US',
  name: 'United States of America',
  display: 'USA',
});

const buildCACountry = () => ({
  code: 'CA',
  name: 'Canada',
  display: 'Canada',
});

const findByCode = async (code) => new Promise((resolve, reject) => {
  switch (code) {
    case 'US':
      return resolve(buildUSCountry());
    case 'CA':
      return resolve(buildCACountry());
    default:
      return reject(Error(`Cannot found country with code ${code}`));
  }
});
module.exports.findByCode = findByCode;
