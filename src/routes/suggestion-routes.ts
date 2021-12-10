import express from 'express';
import { RedisClient } from '../clients/redis-client';
import { ElasticSearchClient } from '../clients/elasticsearch-client';
import { SuggestionController } from '../controllers/suggestion-controller';
import { SuggestionService } from '../services/suggestion-service';
import { suggestionSchema } from '../schemas/suggestion-schema';
import { SuggestionMiddleware } from '../middlewares/suggestion-middleware';
import { asyncHandler } from '../middlewares/async-handler-middleware';

// eslint-disable-next-line new-cap
const router = express.Router();

const redisClient = new RedisClient();
const esClient = new ElasticSearchClient();

const suggestionService = new SuggestionService(esClient);
const suggestionController = new SuggestionController(suggestionService, redisClient);
const suggestionMiddleware = new SuggestionMiddleware(redisClient);

router.get(
  '/',
  [suggestionSchema, suggestionMiddleware.checkSuggestionCache],
  asyncHandler(suggestionController.getSuggestions),
);

export default router;
