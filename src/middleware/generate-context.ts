import { Request, Response, NextFunction } from 'express';
import { PrismaClient } from '@prisma/client';

export async function generateContext(
  req: Request,
  res: Response,
  next: NextFunction
) {
  req.context = { prisma: new PrismaClient() };

  next();
}
