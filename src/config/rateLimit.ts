import { Express } from 'express';
import env from './env';
import { rateLimit } from 'express-rate-limit';

export function setupRateLimit(app: Express) {
  const limiter = rateLimit({
    windowMs: 2 * 60 * 1000,
    max: 20,
    standardHeaders: true,
    legacyHeaders: false,
  });

  app.use(limiter);
  app.set('trust proxy', env.server.trustProxy);
}
