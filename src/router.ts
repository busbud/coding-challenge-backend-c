import { Router } from 'express';
import { query } from 'express-validator';

import { handleInputErrors } from './modules/middleware';
import { coordinateValidator } from './validators/suggestions';
import { getSuggestions } from './handlers/suggestions';

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
  handleInputErrors,
  getSuggestions,
);

export default router;
