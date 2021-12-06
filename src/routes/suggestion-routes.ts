import express from 'express';
import { ElasticSearchClient } from '../clients/elasticsearch-client';
import { SuggestionController } from '../controllers/suggestion-controller';
import { SuggestionService } from '../services/suggestion-service';

// eslint-disable-next-line new-cap
const router = express.Router();

const esClient = new ElasticSearchClient();
const suggestionService = new SuggestionService(esClient);
const suggestionController = new SuggestionController(suggestionService);

router.post('/', suggestionController.getSuggestions);

export default router;
