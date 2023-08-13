import { PrismaClient } from '@prisma/client';
import { RedisClient } from '../../config/redis';

export interface Context {
  prisma: PrismaClient;
  redis: RedisClient;
}

declare global {
  namespace Express {
    interface Application {
      redis: RedisClient;
      disconnect: () => void;
    }

    interface Request {
      context: Context;
    }
  }
}
