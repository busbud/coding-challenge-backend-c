import Koa from 'koa';
import Router from 'koa-router';
import baseRouter from 'router/base';
import docRouter from 'router/doc';
import suggestionRouter from 'router/suggestion';

const app = new Koa();

const router = new Router();

router
  .use(baseRouter)
  .use(docRouter)
  .use(suggestionRouter);

app.use(router.routes());

export default app;
