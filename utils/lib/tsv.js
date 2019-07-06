const fs = require('fs');
const readline = require('readline');

const parse = async (tsvFile) => {
    const fileStream = fs.createReadStream(tsvFile);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    headers = null;
    elasticData = [];

    //Iterate over every line in the file and build a query for elasticsearch
    for await (const line of rl) {
        const data = line.split('\t');
        if (headers === null) {
            headers = data;
        } else {
            lat = null;
            lng = null;
            elasticData.push({ index: { _index: 'population', _id: data[0] } });
            
            //Extract lat and long and create get_point in elasticsearch
            //The rest are extracted from the headers in the tsv file
            const body = headers.reduce((obj, nextKey, index) => {
                if (nextKey === 'lat') {
                    lat = Number(data[index]);
                } else if (nextKey === 'long') {
                    lng = Number(data[index]);
                }
                else {
                    obj[nextKey] = data[index];
                }
                if (lat !== null && lng !== null) {
                    obj['location'] = { "lat": lat, "lon": lng };
                }
                return obj;
            }, {});
            elasticData.push(body);
        }
    }
    console.log("Parsed file: " + elasticData.length);
    return elasticData;
}

module.exports = {
    parse: parse
}