import Router from 'koa-router';
import baseController from 'controller/base';

const router = new Router();

router.get('/', baseController.ping);

export default router.routes();
