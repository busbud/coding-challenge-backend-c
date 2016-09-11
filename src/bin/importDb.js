import 'babel-polyfill'
import importManager from '../data/importer'
import redis from '../data/datasource/connection'

const DEFAULT_FILE = {
  name: 'cities_canada-usa.tsv',
  // path: `${__dirname}/../../test/utils/fixtures/import-cities-1.tsv`
  path: `${__dirname}/../../data/cities_canada-usa.tsv`
}

const timerStart = () => {
  const tStart = process.hrtime()
  const ms = (hrTime) => hrTime[0] * 1000 + hrTime[1] / 1000000

  return (fn) => {
    const tEnd = process.hrtime()
    const tEndMs = ms(tEnd)
    const tStartMS = ms(tStart)
    return fn ? fn(tStart, tEnd) : tEndMs - tStartMS
  }
}

const report = (results) => {
  const totalImported = results.reduce((c, f) => {
    c += f.length
    return c
  }, 0)
  console.log('%s rows imported, in %s ms', totalImported, timerEnd())
}

const timerEnd = timerStart()

console.log('Pulling data from %s', DEFAULT_FILE.path)
importManager.importFromFile(DEFAULT_FILE)
  .then(report)
  .then(() => redis.quit())
  .catch(console.error)
