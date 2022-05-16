import Router from 'koa-router';
import suggestionController from 'controller/suggestion';

const router = new Router();

router.get('/suggestions', suggestionController.suggestions);

export default router.routes();
