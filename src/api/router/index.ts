import { Router } from 'express';
import { SuggestionController } from 'api/controllers/suggestion';
import { Connection } from 'api/db';

export function makeRouter(connection: Connection) {
  const router = Router();
  const suggestionsController = new SuggestionController(connection);
  router.get('/suggestions', suggestionsController.get);
  return router;
}
