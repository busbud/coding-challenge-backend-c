import Router from 'koa-router';
import { koaSwagger } from 'koa2-swagger-ui';
import swaggerConfig from 'config/swagger';
import docController from 'controller/doc';

const swaggerJSDoc = require('swagger-jsdoc');

const router = new Router();

router.get('/doc', koaSwagger({
  routePrefix: false,
  hideTopbar: true,
  swaggerOptions: { spec: swaggerJSDoc(swaggerConfig) },
}));

router.get('/doc/swagger.json', docController.swagger);

export default router.routes();
