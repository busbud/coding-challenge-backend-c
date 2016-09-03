import diacritics from 'diacritics'

export default (redis, prefix = 'search') => ({
  query(key, input, limit = 50) {
    const ninput = diacritics.remove(input).toLowerCase()

    return redis.zrangebylex(`${prefix}:${key}`, `[${ninput}`, `[${ninput}ÿ`, 'limit', 0, limit)
      .then((found) => {
        const pipeline = redis.pipeline()
        found.forEach((s) => pipeline.smembers(`${prefix}:${key}:${s}`))
        return pipeline.exec()
      })
      .then((results) => [].concat(...results.map((r) => r[1])))
  },

  index(key, value, id) {
    const multi = redis.multi()
    const nvalue = diacritics.remove(value).toLowerCase()
    multi.zadd(`${prefix}:${key}`, 0, `${nvalue}`)
    multi.sadd(`${prefix}:${key}:${nvalue}`, id)
    return multi.exec()
  }
})
