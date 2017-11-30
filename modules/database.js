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
                        const city = {
                            id: chunk.id,
                            rawName: chunk.name,
                            name: `${chunk.name}, ${chunk.admin1}, ${chunk.country}`,
                            latitude: chunk.lat,
                            longitude: chunk.long,
                            country: chunk.country,
                            code: chunk.admin1
                        };
                        this.push(city);
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
            //filter data with city name
            const filterDataTransformer = filterData(filter);
            //cumulate data to return
            const cumulateDataTransformer =
                es.mapSync((data) => {
                    suggestions.push(data);
                });
            //pipe transformers
            pipeTransformers([filterDataTransformer, ...transformers, cumulateDataTransformer]);
            //read stream and go !
            fs.createReadStream(dataFilename)
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
                .pipe(filterDataTransformer);
        });
    };

    const pipeTransformers = (transformers) => {
        for (let i = 0; i < transformers.length - 1; i++) {
            transformers[i].pipe(transformers[i + 1]);
        }
    };

    return {
        queryData: queryData
    }
})();
