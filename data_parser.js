const Readlines = require('n-readlines')
const path = require('path')
const caCodes = require('./data/ca_codes')

const dataDir = path.join(__dirname, 'data')
const dataFile = path.join(dataDir, 'cities_canada-usa.tsv')

// parse the .tsv file and return the relevant information in an array
const generateData = () => {
    const results = []
    let firstLine = true
    let fileHeaders = []
    const relevantHeaders = new Set(['id', 'name', 'lat', 'long', 'country', 'admin1', 'population'])

    let line
    const liner = new Readlines(dataFile)

    while (line = liner.next()) {
        line = line.toString()
        if (firstLine) {
            firstLine = false
            fileHeaders = line.split('\t')
        } else {
            const lineObj = {}
            const lineArray = line.split('\t')
            fileHeaders.map((v, i) => { if (relevantHeaders.has(v)) { lineObj[v] = lineArray[i] } })
            if (lineObj.country === 'CA') { lineObj.admin1 = caCodes.convert([lineObj.admin1]) }
            results.push(lineObj)
        }
    }
    return results
}

module.exports = { generateData }
