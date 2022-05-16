import { Context } from 'koa';
import swaggerConfig from 'config/swagger';

const swaggerJSDoc = require('swagger-jsdoc');

export default {
  async swagger(ctx: Context) {
    ctx.body = swaggerJSDoc(swaggerConfig);
  },
};
