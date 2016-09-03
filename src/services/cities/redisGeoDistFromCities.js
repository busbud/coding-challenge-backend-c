import dataSource from '../../data/datasource'
import { concatReplies } from './utils'

const DEFAULT_OPTIONS = {
  unit: 'km',
  fromKey: 'fromKey',
}

const getId = (long, lat, cities) => {
  const toEncode = [long, lat].concat(...cities.map((c) => c.id)).toString('base64')
  const buffer = Buffer(toEncode)

  return buffer.toString('base64')
}

export default (long, lat, cities, options = DEFAULT_OPTIONS) => {
  const {redis} = dataSource
  const keyId = getId(long, lat, cities)

  const createNewGeoSet = () => {
    const multi = redis.multi()
    multi.geoadd(`cities:geo:dist:${keyId}`, long, lat, options.fromKey)
    cities.forEach((c) => {
      multi.geoadd(`cities:geo:dist:${keyId}`, c.long, c.lat, c.id)
    })
    return multi.exec()
  }

  const searchDistance = () => {
    const pipeline = redis.pipeline()

    cities.forEach((c) => {
      pipeline.geodist(`cities:geo:dist:${keyId}`, options.fromKey, c.id, options.unit)
    })

    pipeline.expire(`cities:geo:dist:${keyId}`, 10)
    return pipeline.exec()
      .then(concatReplies)
      .then((results) => {
        results.pop()
        return results
      })
  }

  const distanteProps = (value) => ({
    distance: {
      value: parseFloat(value),
      unit: options.unit
    }
  })

  return redis.exists(`cities:geo:dist:${keyId}`)
    .then((i) => Boolean(i))
    .then((exist) => {
      if (!exist) {
        return createNewGeoSet()
      }
    })
    .then(searchDistance)
    .then((dists) => {
      return cities.map((c, i) => Object.assign({}, c, distanteProps(dists[i])))
    })
}