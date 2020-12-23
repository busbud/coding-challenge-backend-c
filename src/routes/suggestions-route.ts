import express, { Request, Response } from 'express';
import { query } from 'express-validator';
import { validateRequest } from '../middlewares/validate-request';
import FindCityService from '../services/get-suggestion-service';

const router = express.Router();

router.get(
  '/suggestions',
  [
    query('q').notEmpty().withMessage('location name query string required'),
    query('latitude').optional(true),
    query('longitude').optional(true),
  ],
  validateRequest,
  async (request: Request, response: Response) => {
    const { q, latitude, longitude } = request.query;
    const findCityService = new FindCityService();
    const suggestion = await findCityService.execute({
      requestQuery: q,
      lat: latitude,
      long: longitude,
    });
    return response.json({ suggestions: suggestion }).status(200);
  },
);

export { router as suggestions };
