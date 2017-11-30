const fs = require("fs");
const es = require("event-stream");
const csv = require('csv-parser');
const miss = require("mississippi");

module.exports = (() => {
    const dataFilename = "./data/cities_canada-usa.tsv";

    const filterData = (filter) => {
        return miss.through(
            {objectMode: true},
            function(chunk, encode, callback) {
                //Add cities with a population above 5000
                if (chunk.population && parseInt(chunk.population, 10) > 5000) {
                    //filter by city name
                    if (chunk.name.toLowerCase().includes(filter)) {
                        this.push({
                            id: chunk.id,
                            rawName: chunk.name,
                            name: `${chunk.name}, ${chunk.admin1}, ${chunk.country}`,
                            latitude: chunk.lat,
                            longitude: chunk.long,
                            country: chunk.country,
                            code: chunk.admin1
                        });
                    }
                }
                callback();
            },
            (callback) => {
                callback();
            });
    };

    const queryData = (filter, transformers) => {
        filter = filter.toLowerCase();
        return new Promise((resolve, reject) => {
            const suggestions = [];
            const stream = fs.createReadStream(dataFilename)
                .on("end", () => {
                    resolve(suggestions);
                })
                .on("error", (err) => {
                    reject(err);
                })
                .pipe(csv({
                    separator: '\t',
                    //string that can't be find in the file (a bit weird yes, but file contains " and it breaks the process...)
                    quote: "@;#&%"
                }))
                .pipe(filterData(filter));

            //Pipe others transformers
            transformers.forEach((transformer) => {
                stream.pipe(transformer);
            });
            //cumulate data to return
            stream.pipe(es.mapSync((data) => {
                suggestions.push(data);
            }))
        });
    };

    return {
        queryData: queryData
    }
})();
