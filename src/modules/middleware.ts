import { validationResult } from 'express-validator';
import rateLimit from 'express-rate-limit';
import { cleanEnv, port, str } from 'envalid';

import type { Request, Response, NextFunction } from 'express';

const rateLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // Limit each IP to 100 requests per `window` (here, per 15 minutes)
  message: 'You have exceeded the 100 requests in 15 minutes limit!',
  standardHeaders: true, // Return rate limit info in the `RateLimit-*` headers
  legacyHeaders: false, // Disable the `X-RateLimit-*` headers
});

const handleInputErrors = (req: Request, res: Response, next: NextFunction) => {
  const result = validationResult(req);

  if (result.isEmpty()) return next();

  const errors = result.array().map((error) => error.msg);
  res.status(400).send({ errors });
};

const validateEnv = () => {
  if (process.env.NODE_ENV === 'development') {
    cleanEnv(process.env, {
      POSTGRES_HOST: str(),
      POSTGRES_PORT: port(),
      POSTGRES_USER: str(),
      POSTGRES_PASSWORD: str(),
      POSTGRES_DB: str(),
    });
  }

  cleanEnv(process.env, {
    DOMAIN: str(),
    DATABASE_URL: str(),
    MORGAN_FORMAT: str(),
    PORT: port(),
    REDIS_URL: str(),
  });
};

export { rateLimiter, handleInputErrors, validateEnv };
