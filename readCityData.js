const fs = require("fs");
const readLine = require("readline");

module.exports = { readCityData };

async function readCityData(filePath) {
    const cityDataStream = fs.createReadStream(filePath, { encoding: "utf-8" });
    const lineReader = readLine.createInterface({
        input: cityDataStream,
        crlfDelay: Infinity
    });
    let isFirstLine = true;
    const cityHeaders = [];
    const citiesData = [];
    for await (const line of lineReader) {
        const lineParts = line.split("\t");
        if (isFirstLine) {
            cityHeaders.push(...lineParts);
            isFirstLine = false;
        } else if (cityHeaders.length !== lineParts.length){
            console.warn(`Invalid data package ${JSON.stringify(lineParts)}`);
        } else {
            citiesData.push(parseCityData(cityHeaders, lineParts));
        }
    }

    return citiesData;
}

function parseCityData(dataHeaders, data) {
    const cityData = {};
    if (data.length !== dataHeaders.length) {
        throw new Error("Data headers and data package are not");
    }
    for (let i = 0; i < dataHeaders.length; i++) {
        const field = dataHeaders[i];
        cityData[field] = data[i];
    }
    
    return cityData;
}