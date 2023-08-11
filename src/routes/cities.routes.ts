import { getCitiesSuggestionsController } from '../controllers/cities/get-cities-suggestions.controller';
import { Router } from 'express';

export default function (router: Router) {
  router.get('/suggestions', getCitiesSuggestionsController);
}
