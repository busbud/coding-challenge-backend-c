import dotenv from 'dotenv';
import nodeConfig, { Env } from 'config/node';

dotenv.config();

export default {
  bypass: nodeConfig.env === Env.test || Boolean(Number(process.env.CACHE_BYPASS)),
};
