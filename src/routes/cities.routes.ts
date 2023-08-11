import { getCitySuggestions } from '../controllers/cities/get-suggestions.controller';
import { Router } from 'express';

export default function (router: Router) {
  router.get('/suggestions', getCitySuggestions);
}
