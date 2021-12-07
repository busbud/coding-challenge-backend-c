import { createClient } from 'redis';

const { APP_ENV } = process.env;

export class RedisClient {
  private redisClient: any;

  constructor() {
    let redisClientOptions;

    if (APP_ENV === 'production') {
      redisClientOptions = {
        url: process.env.REDIS_URL,
        password: process.env.REDIS_PASSWORD,
      };
    }

    this.redisClient = createClient(redisClientOptions);
    this.redisClient.connect();
  }

  getKey = async <T>(key: string): Promise<T> => {
    const result: string = await this.redisClient.get(key);
    return JSON.parse(result);
  };

  setKey = async (key: string, data: any) => this.redisClient.set(key, JSON.stringify(data));
}
