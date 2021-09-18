import { Router } from 'express';
import { makeSuggestionController } from 'api/controllers/suggestion';
import { Connection } from 'api/db';

export function makeRouter(connection: Connection) {
  const router = Router();
  const suggestionsController = makeSuggestionController(connection);
  router.get('/suggestions', suggestionsController.get);
  return router;
}
