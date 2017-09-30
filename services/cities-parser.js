var fs = require('fs');
var es = require('event-stream');

var getData = (fileName) => {

    // Return promise, async handling so we have the final data
    return new Promise((resolve, reject) => {
  
        let data = [];

        // Read TSV file
        fs.createReadStream(`data/${fileName}.tsv`)
            // Split Strings
            .pipe(es.split("\n"))
            // Split Strings into Array
            .pipe(es.mapSync(line => {

                // @todo : skip first line / header

                // Get rid of non-sense / corrupt data
                if(line.length >= 1) {
                    // Format line in a array
                    line =  line.split("\t");
                    // Get needed fields
                    let name = line[1];
                    let location = [parseFloat(line[4]), parseFloat(line[5])]
                    // let latitude = parseFloat(line[4]);
                    // let longitude = parseFloat(line[5]);
                    // Create object line by line
                    // line = { name, latitude, longitude };
                    line = { name };
                    data.push(line);
                }

            }))
            .on('close', () => {
                resolve(data);
            });
    });
  }

module.exports = getData;
