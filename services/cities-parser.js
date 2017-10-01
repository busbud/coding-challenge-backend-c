const fs = require('fs');
const es = require('event-stream');
const validator = require('./../services/validator');


exports.getData = function(fileName) {

    // Return promise, async handling so we have the final data
    return new Promise((resolve, reject) => {
  
        let data = [];

        // Read TSV file
        fs.createReadStream(`data/${fileName}.tsv`)
            // Split Strings
            .pipe(es.split("\n"))
            // Split Strings into Array
            .pipe(es.mapSync(line => {
                // Skip potentials fucked-up lines and first line / header
                if(line.length >= 1) {
                    // Format line in a array
                    line =  line.split("\t");
                    
                    if(lineIsValid(line)) {
                        // Get needed fields
                        let name = line[1];
                        let location = [parseFloat(line[4]), parseFloat(line[5])]
                        let country = line[8];
                        let admin1 = country == 'CA' ? getProvince(parseInt(line[10])) : line[10];

                        line = { name, location, admin1, country };
                        data.push(line);
                    }
                }
            }))
            .on('error', error => {
                reject(error);
            })
            .on('close', () => {
                resolve(data);
            });
    });
}

function getProvince(provinceCode) {

    const provinces = {
        1:'AB',
        2:'BC',
        3:'MB',
        4:'NB',
        5:'NL',
        7:'NS',
        8:'ON',
        9:'PE',
        10:'QC',
        11:'SK',
        12:'YT',
        13:'NT',
        14:'NU'
    }

    return provinces[provinceCode] || '';
}

function lineIsValid(line) {
    // Only checks both latitude and longitude for now
    return validator.coordinateIsValid(line[4]) && validator.coordinateIsValid(line[5]);
}
