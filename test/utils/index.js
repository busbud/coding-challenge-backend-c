import path from 'path'
import dataSource from '../../src/data/dataSource'
import importManager from '../../src/data/importer'

const env = process.env.NODE_ENV

export const flushDB = () => {
  if (env ===  'development' || env === 'test') {
    return dataSource.redis.flushdb()
  }
  throw new Error('flushAll is not permitted, too dangerous')
}

export const importDB = () => {
  return importManager.importFromFile({
    name: 'cities_canada-usa.tsv',
    path: `${__dirname}/../../data/cities_canada-usa.tsv`
  })
}