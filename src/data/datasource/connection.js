import Redis from 'ioredis'
import conf from '../../config'

const host = conf.get('REDIS_URL')
const redis = new Redis(host)

export default redis
