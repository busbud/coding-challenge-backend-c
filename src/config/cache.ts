import dotenv from 'dotenv';
import nodeConfig, { Env } from 'config/node';
import BypassCache from 'service/cache/bypass';
import RedisCache from 'service/cache/redis';

dotenv.config();

export default {
  AdapterClass: nodeConfig.env === Env.test || Boolean(Number(process.env.CACHE_BYPASS))
    ? BypassCache
    : RedisCache,
};
