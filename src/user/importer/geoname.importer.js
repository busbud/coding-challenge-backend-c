const fs = require('fs');
const readline = require('readline');
const countryAdapter = require('../../infrastructure/country.adapter')
const divisionsAdapter = require('../../infrastructure/administrative.division.adapter')
const suggestionCommand = require('../../command/suggestions.command')
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

const importCsv = async (filepath) => {
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
        await processLine(parsedLine).catch(reason => console.log(reason))
    }
}

const processLine = async (parsedLine) => new Promise(async (resolve, reject) => {
    const suggestion = await buildSuggestion(parsedLine).catch(reason => reject(reason))
    const saved = await suggestionCommand.createOrUpdate(suggestion).catch(reason => reject(reason))
    resolve(saved)
})

const buildSuggestion = async (parsedLine) => new Promise(async (resolve, reject) => {

    const country = await countryAdapter.findByCode(parsedLine['country_code'])
        .catch(reason => reject(reason))

    const divisionCode = await divisionsAdapter.findCodeByFipsCode(parsedLine['country_code'], parsedLine['admin1_code'])
        .catch(reason => reject(reason))

    resolve({
        id: parsedLine.geonameid,
        fullSuggestions: `${parsedLine.name}, ${divisionCode}, ${country.display}`,
        name: parsedLine.name,
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

module.exports = importCsv
