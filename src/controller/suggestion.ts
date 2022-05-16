import { Context } from 'koa';
import joi from 'joi';
import suggestion from 'service/suggestion';

export default {
  async suggestions(ctx: Context) {
    const schema = joi.object({
      q: joi.string()
        .min(1)
        .required(),
      latitude: joi.number()
        .greater(-90)
        .less(90),
      longitude: joi.number()
        .greater(-180)
        .less(180),
    })
      .with('latitude', ['longitude'])
      .with('longitude', ['latitude']);

    const validation = schema.validate({
      q: ctx.request.query.q,
      latitude: ctx.request.query.latitude,
      longitude: ctx.request.query.longitude,
    });

    if (validation.error) {
      ctx.body = { error: validation.error.message };
      ctx.status = 400;
    } else {
      const suggestions = await suggestion.suggestCities(
        String(validation.value.q),
        validation.value.latitude && validation.value.longitude ? {
          latitude: Number(validation.value.latitude),
          longitude: Number(validation.value.longitude),
        } : undefined,
      );
      ctx.body = { suggestions };
      ctx.status = suggestions.length ? 200 : 404;
    }
  },
};
