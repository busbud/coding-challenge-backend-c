import Redis from 'ioredis'
import conf from '../../config'

const host = conf.get('APP_REDIS')
const redis = new Redis({
  host
})

export default redis