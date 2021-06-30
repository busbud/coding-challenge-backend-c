const parse = require('csv-parse')
const fs = require('fs')
const path = require('path')
const { countryFromCode, regionFromFIPS } = require('./helpers')

module.exports = async (client) => {
  const bulk = []
  const filePath = path.join(__dirname, 'data', 'canada_usa_seed.tsv')
  const parser = fs.createReadStream(filePath)
    .pipe(parse({ delimiter: '\t', columns: true, relax: true, quote: false }))

  for await (const record of parser) {
    bulk.push({
      index: {
        _index: 'cities',
        _id: record.id
      }
    })

    const country = countryFromCode(record.country)
    const region = regionFromFIPS(record.admin1)

    bulk.push({
      displayName: `${record.name}, ${region}, ${country}`,
      name: record.name,
      country,
      region,
      location: {
        lat: parseFloat(record.lat),
        lon: parseFloat(record.long)
      }
    })
  }
  return client.bulk({ body: bulk })
}
