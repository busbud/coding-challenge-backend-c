const mongoose = require('mongoose');
const CityModel = require('./city');
const ConstantModel = require('./constant');
const installDefaults = require('./installDefaults');

module.exports = function(config) {
    mongoose.connect(config.db);
    const db = mongoose.connection;
    db.on('error', err => console.error(err));
    db.once('open', ()=> console.log("database connection established"));

    installDefaults
        .checkForCities()
            .then(() => {
                console.log('Check for Cities completed');
                installDefaults
                    .checkForMaxAndMinDistance()
                    .then(() => console.log('Check for Min and Max Completed'))
                    .catch(merr=> console.error(merr));
            })
            .catch(err=> console.error(err));

    return {
        'db': db,
        'CityModel': CityModel,
        'ConstantModel': ConstantModel
    };
};