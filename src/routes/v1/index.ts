import { Router } from 'express';
import rateLimit from 'express-rate-limit';
import suggestions from './suggestions';

/**
 * I like smaller rate limit windows better handles burst traffic from malicious users, and shuts them down faster.
 */

const rateLimiter = rateLimit({
  windowMs: 1e3 * 30, // 30 seconds
  max: 10, // 10 requests per windowMs
  message: 'Rate limit exceeded', // I would likely send localization key here for translations.
});

export default function (): Router {
  const router = Router();

  router.use(rateLimiter);

  router.use('/suggestions', suggestions());

  return router;
}
