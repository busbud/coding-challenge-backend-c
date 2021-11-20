import * as joi from '@hapi/joi';
export const suggestionsQueryJoiSchema = joi.object().keys({
  q: joi.string().required(),
  latitude: joi.number().min(0).max(90).optional(),
  longitude: joi.number().min(-180).max(180).optional()
}).unknown(true);