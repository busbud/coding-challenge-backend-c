import dataSource from '../../data/datasource'
import indexSearch from './redisIndexSearch'

export default (city) => {
  const {redis} = dataSource

  const indexCity = (isNew) => {
    const multi = redis.multi()

    if (!isNew) {
      multi.incr('cities:count')
    }

    multi.sadd(`cities:ids`, city.id)
    multi.geoadd('cities:geo', city.long, city.lat, city.id)
    multi.hmset(`city:${city.id}`, city)
    return multi.exec()
  }

  return redis.sismember('cities:ids', city.id)
    .then((i) => Boolean(!i))
    .then((isNew) => indexCity(isNew))
    .then(() => indexSearch(city))
    .then(() => city)
}