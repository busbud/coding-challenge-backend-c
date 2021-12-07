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

    this.redisClient = createClient({
      url: process.env.REDIS_URL || 'redis://redis-16820.c17.us-east-1-4.ec2.cloud.redislabs.com:16820',
      password: process.env.REDIS_PASSWORD || 'mdyaxNOfqYQM0e8U4dkd3kuiwpidrP3r',
    });

    this.redisClient.connect();
  }

  getKey = async <T>(key: string): Promise<T> => {
    const result: string = await this.redisClient.get(key);
    return JSON.parse(result);
  };

  setKey = async (key: string, data: any) => this.redisClient.set(key, JSON.stringify(data));
}
