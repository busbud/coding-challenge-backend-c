import { validationResult } from 'express-validator';
import rateLimit from 'express-rate-limit';

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

export { rateLimiter, handleInputErrors };
