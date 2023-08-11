import { PrismaClient } from '@prisma/client';

export interface Context {
  prisma: PrismaClient;
}

declare global {
  namespace Express {
    interface Request {
      context: Context;
    }
  }
}
