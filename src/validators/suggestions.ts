import type { CustomValidator } from 'express-validator';

const coordinateValidator: CustomValidator = (value, { req }) => {
  if (value && req.query) {
    if (!req.query.latitude)
      throw new Error('Latitude is required when providing longitude.');

    if (!req.query.longitude)
      throw new Error('Longitude is required when providing latitude.');
  }

  return true;
};

export { coordinateValidator };
