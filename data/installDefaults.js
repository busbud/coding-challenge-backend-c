const fs = require('fs');
const csv = require('csv-streamify');
const split = require('split');
const CityModel = require('./city');
const ConstantModel = require('./constant');

const parser = csv({
  delimiter: '\t',
});


// function installDefaults(){
//   return new Promise((resolve, reject) => {
//     const records = [];
//     // emits each line as a buffer or as a string representing an array of fields
//     parser.on('data', function (line) {
//       // if (line.length > 0 && line[14] >= 5000){
//         if (line.length > 0){
//         let city = new CityModel({
//           geonameid: line[0],
//           name: line[1],
//           asciiname: line[2],
//           latitude: line[4],
//           longitude: line[5],
//           country_code: line[8],
//           display_name: `${line[1]}, ${line[8]}`
//         });
//         // save the record to the mongo db
//         city.save();
//
//         records.push(city);
//       }
//     });
//
//     parser.on('end', () => {
//       console.log(`We found ${records.length} records`);
//       resolve(records);
//     });
//
//     parser.on('error', (err) => reject(err));
//
//     // now pipe some data into it
//     fs.createReadStream('./data/cities_canada-usa.tsv')
//       .pipe(parser);
//   });
// }


function installDefaults() {
  return new Promise((resolve, reject) => {
    const records = [];
    fs.createReadStream('./data/cities_canada-usa.tsv')
        .pipe(split())
        .on('data', line => {
          const rec = line.split('\t');
          if (rec.length > 0 && rec[14] >= 5000){
          // if (rec.length > 0){
            // Assign to the City Model
            if (rec[0] !== 'id'){
              let city = new CityModel({
                geonameid: rec[0],
                name: rec[1],
                asciiname: rec[2],
                latitude: rec[4],
                longitude: rec[5],
                country_code: rec[8],
                display_name: `${rec[1]}, ${rec[8]}`
              });
              // save the record to the mongo db
              city.save();
              // add to collection
              records.push(city);
            }
          }
        })
        .on('end', () => {
          console.log("received %s records", records.length);
          resolve(records)
        })
        .on('error', err => reject(err))
  });
}

// installDefaults();

function checkForCities() {
  return new Promise((resolve, reject) => {
    CityModel.find({}).exec((err, collection) => {
      if (!err){
        // If no records received, then launch process to store records from
        // tsv file to the database
        if (collection.length === 0){ 
          installDefaults()
            .then(recs => resolve(`${recs.length} records were created`))
            .catch(err => reject(err));
        }else{ // If records exists, then notify requesting service
          resolve(`${collection.length} city records exist in the database`);
        }
      }else {
        reject(err);
      }
    });
  });
}

const getDistance = require('../utils').getDistance;

function findMaxAndMinDistance() {
  return new Promise((resolve, reject) => {
    console.log("Attempting to generate the maximum and minimum distances of cities");

    let minDistance = Infinity;
    let maxDistance = -1;

    CityModel.find({}).exec((err, collection) => {
      collection.forEach((city1, index1) => {
        collection.forEach((city2, index2) => {
          if (index2 !== index1) {
            const diff = Math.abs(getDistance(city1.latitude, city1.longitude, city2.latitude, city2.longitude));
            if (diff > maxDistance)maxDistance = diff;
            if (diff < minDistance)minDistance = diff;
          }
        })
      });

      console.log(`Max: ${maxDistance}, Min: ${minDistance}`);

      resolve({
        'min': minDistance,
        'max': maxDistance
      });
    });
  });
}

function checkForMaxAndMinDistance() {
  return new Promise((resolve, reject) => {
    ConstantModel.find({name: 'maxDistance'}).exec((err, collection) =>{
      if (!err){
        if (collection.length === 0) {
          console.log("Min and Max values not detected");
          findMaxAndMinDistance()
              .then(res=> {
                const values = [];

                let max = new ConstantModel({
                  'name': 'maxDistance',
                  "value": res['max']
                });
                max.save();
                values.push(max);

                let min = new ConstantModel({
                  'name': 'minDistance',
                  "value": res['min']
                });
                min.save();
                values.push(min);

                resolve(values);
              })
              .catch(merr => reject(merr));
        }else{
          console.log("Min and Max values were previously created");
          resolve(collection);
        }
      }else{
        reject(err);
      }
    });
  });
}

module.exports = {
  'checkForCities': checkForCities,
  'installCities': installDefaults,
  'findMaxAndMinDistance': findMaxAndMinDistance,
  'checkForMaxAndMinDistance': checkForMaxAndMinDistance
};