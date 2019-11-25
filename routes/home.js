/* eslint-disable no-return-assign */
const Router = require('koa-router');

const router = Router();

router.get('/', (ctx) => {
  ctx.body = { data: 'Health check ok!' };
});

module.exports = router;
