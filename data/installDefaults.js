const fs = require('fs');
const split = require('split');
const CityModel = require('./city');
const ConstantModel = require('./constant');

function installDefaults() {
    return new Promise((resolve, reject) => {
        const records = [];
        fs.createReadStream('./data/cities_canada-usa.tsv')
            .pipe(split())
            .on('data', line => {
                const rec = line.split('\t');
                if (rec.length > 0 && rec[14] >= 5000) {
                    // if (rec.length > 0){
                    // Assign to the City Model
                    if (rec[0] !== 'id') {
                        let city = new CityModel({
                            geonameid: rec[0],
                            name: rec[1],
                            asciiname: rec[2],
                            latitude: rec[4],
                            longitude: rec[5],
                            country_code: rec[8],
                            display_name: `${rec[2]}, ${rec[8]}`
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

function checkForCities() {
    return new Promise((resolve, reject) => {
        CityModel.find({}).exec((err, collection) => {
            if (!err) {
                // If no records received, then launch process to store records from
                // tsv file to the database
                if (collection.length === 0) {
                    installDefaults()
                        .then(recs => resolve(`${recs.length} records were created`))
                        .catch(err => reject(err));
                } else { // If records exists, then notify requesting service
                    resolve(`${collection.length} city records exist in the database`);
                }
            } else {
                reject(err);
            }
        });
    });
}

function findMaxAndMinDistance() {
    const getDistance = require('../utils').getDistance;

    return new Promise((resolve, reject) => {
        console.log("Attempting to generate the maximum and minimum distances of cities");
        try{
            let minDistance = Infinity;
            let maxDistance = -1;

            CityModel.find({}).exec((err, collection) => {
                collection.forEach((city1, index1) => {
                    collection.forEach((city2, index2) => {
                        if (index2 !== index1) {
                            const diff = Math.abs(getDistance(city1.latitude, city1.longitude, city2.latitude, city2.longitude));
                            if (diff > maxDistance) maxDistance = diff;
                            if (diff < minDistance) minDistance = diff;
                        }
                    })
                });

                console.log(`Completed Scan. Found Max: ${maxDistance} and Min: ${minDistance}`);

                resolve({
                    'min': minDistance,
                    'max': maxDistance
                });
            });
        }catch (e) {
            reject(e);
        }


    });
}

function checkForMaxAndMinDistance() {
    return new Promise((resolve, reject) => {
        ConstantModel.find({name: 'maxDistance'}).exec((err, collection) => {
            if (!err) {
                if (collection.length === 0) {
                    console.log("Min and Max values not detected");
                    findMaxAndMinDistance()
                        .then(async res => {
                            const values = [];

                            let max = new ConstantModel({
                                'name': 'maxDistance',
                                "value": res['max']
                            });
                            await max.save();
                            values.push(max);

                            let min = new ConstantModel({
                                'name': 'minDistance',
                                "value": res['min']
                            });
                            await min.save();
                            values.push(min);

                            resolve(values);
                        })
                        .catch(merr => reject(merr));
                } else {
                    console.log("Min and Max values were previously created");
                    resolve(collection);
                }
            } else {
                reject(err);
            }
        });
    });
}

function main() {
    const env = process.env.NODE_ENV = process.env.NODE_ENV || 'development';
    const config = require('./config')[env];
    require('./db')(config).then((db) => {
        console.log('Connected in the install defaults');
        checkForCities()
            .then(() => {
                console.log('Check for cities completed');
                checkForMaxAndMinDistance()
                    .then(() => {
                        console.log('Check for Min and Max completed');
                        db.close();
                    })
                    .catch(merr => console.error(merr));
            })
            .catch(err => console.error(err));
    });
}

main();