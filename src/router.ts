import { Router } from 'express';
import { query, matchedData, validationResult } from 'express-validator';

import { coordinateValidator } from './validators/suggestions';

const router = Router();

router.get('/', (_req, res) =>
  res.json({ message: 'Hey there welcome to Busbud! :)' }),
);

// Suggestions
router.get(
  '/suggestions',
  query('q')
    .notEmpty()
    .isString()
    .escape() // Prevents an XSS
    .withMessage('Query parameter "q" must be a non-empty string.'),
  // Got min and max from here https://docs.mapbox.com/help/glossary/lat-lon/
  query('latitude')
    .optional()
    .isFloat({ min: -90, max: 90 })
    .withMessage('Latitude must be a valid number between -90 and 90.')
    .custom(coordinateValidator),
  query('longitude')
    .optional()
    .isFloat({ min: -180, max: 180 })
    .withMessage('Longitude must be a valid number between -180 and 180.')
    .custom(coordinateValidator),
  (req, res) => {
    const result = validationResult(req);

    if (result.isEmpty()) {
      const data = matchedData(req);
      return res.status(200).json({ data });
    }

    const errors = result.array().map((error) => error.msg);
    return res.status(400).send({ errors });
  },
);

export default router;
