import fs from 'node:fs'

import { parse } from 'csv-parse'

// Load the cities data into memory on startup. This works while the data is
// small enough to not affect app startup time or memory footprint, and not
// changing frequently. If we expected this data to change more often it would
// live in a database.
const c = []
fs.createReadStream('./public/cities_canada-usa.tsv')

  .pipe(parse({
    // the delimiter in the data file is the tab \t char
    delimiter: '\t',
    // " and ' are used in some phonetic spellings of city names, so don't try
    // to parse any quoted fields
    quote: false,
    // return data as objects, using the first row as keys
    columns: true
  }))
  .on('data', row => {
    c.push(row)
  })
  .on('end', () => {
    console.log('  data parsing complete!')
    console.log(JSON.stringify(c))
  })

