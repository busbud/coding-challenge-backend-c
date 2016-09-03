import dataSource from '../../data/datasource'
import redisSearch from '../../lib/search'
import findAllById from './redisFindAllCitiesById'

export default (input = '') => {
  const {redis} = dataSource
  const search = redisSearch(redis)

  return search.query('cities:name', input)
    .then(findAllById)
}
