const fs = require('fs').promises;
const d3 = require('d3');


const file = "./data/cities_canada-usa.tsv";


let cities;
const getAllCities = async () => {
    if (!cities) {
        //else parse it
        try {
            const f = await fs.readFile(file, 'utf8');
            return new Promise((resolve, reject) => {
                let result = d3.tsvParse(f.toString(), (c) => {
                    // reducing footprint by deleting unneeded fields
                    //(should be done by DB in real)
                    return {
                        name: c.name,
                        ascii: c.ascii,
                        country: c.country,
                        code: c.admin1,
                        alternatenames: c.alt_name,
                        lat: c.lat,
                        long: c.long,
                        population: parseInt(c.population)
                    }
                });
                // No need to filter on country or population for these dataset
                cities = result;
                resolve(cities);
            });
        } catch (e) {
            console.error('Error reading tsv file', e);
            return Promise.reject(e);
        }
    }
    // if already loaded, return it
    return Promise.resolve(cities);

}

module.exports = { getAllCities };