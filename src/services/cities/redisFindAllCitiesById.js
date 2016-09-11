import dataSource from '../../data/datasource'
import { concatReplies } from './utils'

export default (ids = []) => {
  const {redis} = dataSource

  const pipeline = redis.pipeline()
  ids.forEach((id) => pipeline.hgetall(`city:${id}`))
  return pipeline.exec()
    .then(concatReplies)
}
