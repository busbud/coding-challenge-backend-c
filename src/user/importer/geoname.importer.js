const fs = require('fs');
const readline = require('readline');
const keys = ['geonameid',
    'name',
    'asciiname',
    'alternatenames',
    'latitude',
    'longitude',
    'feature class',
    'feature code',
    'country code',
    'cc2',
    'admin1 code',
    'admin2 code',
    'admin3 code',
    'admin4 code',
    'population',
    'elevation',
    'dem',
    'timezone',
    'modification date'];

const importCsv = async (filepath) => {
    console.log(filepath)
    const reader = readline.createInterface({
        input: fs.createReadStream(filepath),
        crlfDelay: Infinity
    });

    for await (const line of reader) {
        processLine(parseLine(line))
    }
}

const processLine = (parsedLine) => {
    console.log(parsedLine)
}

const parseLine = (line) => {
    const splitted = line.split("\t")
    let result = {}
    for (let i = 0; i < keys.length; i++) {
        const key = keys[i]
        const value = splitted[i] || null
        result[key] = value
    }

    return result
}

module.exports = importCsv
