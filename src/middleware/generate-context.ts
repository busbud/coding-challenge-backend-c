import { Request, Response, NextFunction } from 'express';
import { PrismaClient } from '@prisma/client';

export async function generateContext(
  req: Request,
  _res: Response,
  next: NextFunction
) {
  const prisma = new PrismaClient();
  const redis = req.app.redis;

  req.context = { prisma, redis };

  next();
}
