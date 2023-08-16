import { validationResult } from 'express-validator';

import type { Request, Response, NextFunction } from 'express';

const handleInputErrors = (req: Request, res: Response, next: NextFunction) => {
  const result = validationResult(req);

  if (result.isEmpty()) return next();

  const errors = result.array().map((error) => error.msg);
  res.status(400).send({ errors });
};

export { handleInputErrors };
