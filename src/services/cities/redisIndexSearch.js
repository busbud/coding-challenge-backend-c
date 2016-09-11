import diacritics from 'diacritics'
import dataSource from '../../data/datasource'
import redisSearch from '../../lib/search'

export default (city) => {
  const {redis} = dataSource

  const search = redisSearch(redis)
  const name = diacritics.remove(city.name)
  return search.index('cities:name', name, city.id)
}
