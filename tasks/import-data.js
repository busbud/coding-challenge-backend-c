const fs = require('fs');
const path = require('path');
const readline = require('readline');
const throat = require('throat');

const db = require('../models');

const readStream = fs.createReadStream(path.join(__dirname, '..', 'data', 'cities_canada-usa.sample.tsv'));

readline
  .createInterface({
    input: readStream,
  })
  .on(
    'line',
    throat(1, async (data) => {
      const [
        idS,
        name,
        ,
        alternateNamesCS,
        latitudeS,
        longitudeS,
        ,
        ,
        countryCode,
        ,
        adminCode,
      ] = data.split(/\t/);

      if (!idS) {
        return;
      }

      await db.City.create({
        id: parseInt(idS, 10),
        countryCode,
        adminCode,
        name,
        alternateNames: alternateNamesCS.split(','),
        latitude: parseFloat(latitudeS),
        longitude: parseFloat(longitudeS),
      });
    }),
  );
