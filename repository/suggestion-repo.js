const fs = require('fs');
const util = require('util');
const path = require("path");

const readFileContent = util.promisify(fs.readFile);

let db = [];

/**
 * 
 * @param {string} cityPartialName 
 * @param {number} latitude 
 * @param {number} longitude 
 */
module.exports.getSuggestions = async (cityPartialName) => {
    cityPartialName = cityPartialName.toUpperCase().replace(' ', '');

    const ret = db.filter(value => value.name.toUpperCase().replace(' ', '').startsWith(cityPartialName));
    
    return ret;
}

const canadaStatesDict = {
    '01': 'AB',
    '02': 'BC',
    '03': 'MB',
    '04': 'NB',
    '05': 'NL',
    '07': 'NS',
    '13': 'NT',
    '14': 'NU',
    '08': 'ON',
    '09': 'PE',
    '10': 'QC',
    '11': 'SK',
    '12': 'VT'
};

(async () => {
    if (db.length === 0) {
        console.info('creating in memory db');

        const buff = await readFileContent(path.resolve(__dirname, '../data/cities_canada-usa.tsv'));

        let items = buff.toString().normalize("NFD").replace(/[\u0300-\u036f]/g, "").split('\n');

        items.forEach(item => {
            const arr = item.split('\t');
            db.push({
                code: arr[0],
                name: arr[1],
                country: arr[8],
                latitude: arr[4], 
                longitude: arr[5],
                population: arr[14],
                state: arr[8] === 'US'? arr[10] : canadaStatesDict[arr[10]]
            });
        });

        db = db.filter((value) =>
            value.name &&
            parseInt(value.population) >= 5000);
    }
})();