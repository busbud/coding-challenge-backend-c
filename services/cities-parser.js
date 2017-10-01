var fs = require('fs');
var es = require('event-stream');

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
                // Skip fucked-up line
                if(line.length >= 1) {
                    // Format line in a array
                    line =  line.split("\t");
                    
                    if(lineIsValid(line)) {
                        // Get needed fields
                        let name = line[1];
                        let location = [parseFloat(line[4]), parseFloat(line[5])]
                        let country = line[8];
                        let state = country == 'CA' ? getState(parseInt(line[10])) : line[10];

                        line = { name, location, state, country };
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

function getState(stateCode) {

    var provinces = {
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

    return provinces[stateCode] || '';
}

function lineIsValid(line) {
    
    // Check both latitude and longitude
    function locationIsvalid(line) {

        if(
            line[4] && line[5] && 
            !isNaN(parseFloat(line[4])) && !isNaN(parseFloat(line[4])) && 
            parseFloat(line[4]) <= 180 && parseFloat(line[5]) <= 180 && 
            parseFloat(line[4]) >= -180 && parseFloat(line[5]) >= -180
        ) {
            return true;
        }
        else {
            return false;
        }
    }

    return locationIsvalid(line)
}
