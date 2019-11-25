/* eslint-disable no-return-assign */
const Router = require('koa-router');

const SuggestionsController = require('../controllers/SuggestionsController');

const router = Router();


router.get('/suggestions', async (ctx) => {
  ctx.body = await SuggestionsController.getSuggestions(ctx);
});

module.exports = router;
