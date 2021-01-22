const fs = require('fs');
const readline = require('readline');
const countryAdapter = require('./country.adapter')
const divisionsAdapter = require('./administrative.division.adapter')
const keys = [
    'geonameid',
    'name',
    'asciiname',
    'alternatenames',
    'latitude',
    'longitude',
    'feature_class',
    'feature_code',
    'country_code',
    'cc2',
    'admin1_code',
    'admin2_code',
    'admin3_code',
    'admin4_code',
    'population',
    'elevation',
    'dem',
    'timezone',
    'modification_date'
];

const performImport = async (filepath, suggestionCallback) => {
    console.log(filepath)
    const reader = readline.createInterface({
        input: fs.createReadStream(filepath),
        crlfDelay: Infinity
    });

    for await (const line of reader) {
        const parsedLine = parseLine(line)
        if (parsedLine.geonameid === 'id') {
            continue
        }
        console.log('Processing suggestion ' + parsedLine.geonameid)
        const suggestion = await buildSuggestion(parsedLine).catch(reason => console.log(reason))
        await suggestionCallback(suggestion).catch(reason => console.log(reason))
    }
}

const processLine = async (parsedLine) => new Promise(async (resolve, zreject) => {
    const suggestion = await buildSuggestion(parsedLine).catch(reason => reject(reason))
    const saved = await suggestionCommand.createOrUpdate(suggestion).catch(reason => reject(reason))
    resolve(saved)
})

const buildSuggestion = async (parsedLine) => new Promise(async (resolve, reject) => {

    const country = await countryAdapter.findByCode(parsedLine['country_code'])
        .catch(reason => reject(reason))

    const divisionCode = await divisionsAdapter.findCodeByFipsCode(parsedLine['country_code'], parsedLine['admin1_code'])
        .catch(reason => reject(reason))

    const name = parsedLine.asciiname;

    resolve({
        id: parsedLine.geonameid,
        fullSuggestion: `${name}, ${divisionCode}, ${country.display}`,
        name: name,
        division: divisionCode,
        country: country,
        location: {
            lat: parsedLine.latitude,
            lon: parsedLine.longitude
        }
    })
})

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

module.exports = performImport
