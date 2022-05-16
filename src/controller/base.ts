import { Context } from 'koa';

export default {
  async ping(ctx: Context) {
    ctx.status = 200;
  },
};
