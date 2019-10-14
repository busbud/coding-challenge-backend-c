const csv = require('csv-parser');
const fs = require('fs');
const filePath = './data/cities_canada-usa.tsv';


module.exports.getCityData = async function () {
    const results = [];
    return new Promise((resolve, reject) => {
        try {
            fs.createReadStream(filePath)
                .on('error', (err) => {
                    reject(err);
                })
                .pipe(csv({ separator: '\t' }))
                .on('data', (data) => {
                    results.push(data)
                })
                .on('end', () => {
                    resolve(results);
                })
        } catch (err) {
            reject(err);
        }
    });

}
