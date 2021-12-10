import { Request, Response, NextFunction } from 'express';
import Joi, { ValidationOptions } from 'joi';

export function suggestionSchema(req: Request, res: Response, next: NextFunction) {
  const schema = Joi.object().keys({
    q: Joi.string().required(),
    latitude: Joi.number(),
    longitude: Joi.number(),
  }).and('latitude', 'longitude');

  const options: ValidationOptions = {
    abortEarly: false,
    allowUnknown: true,
    stripUnknown: true,
  };

  const { error, value } = schema.validate(req.query, options);

  if (error) {
    next({ message: `Validation error: ${error.details.map((err) => err.message).join(', ')}`, statusCode: 400 });
  } else {
    req.query = value;
    next();
  }
}
