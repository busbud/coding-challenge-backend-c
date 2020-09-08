import { Router } from 'express';
import suggestions from './v1/suggestions';
import v1 from './v1';

export default function (): Router {
  const router = Router();

  router.use('/v1', v1());
  router.use('/suggestions', suggestions());
  return router;
}
