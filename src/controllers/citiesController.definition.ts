import Joi from "joi";

export const citySuggestionsSchema = Joi.object()
  .keys({
    cityName: Joi.string().required(),
    longitude: Joi.string().optional().allow(null),
    latitude: Joi.string().optional().allow(null),
  })
  .and("latitude", "longitude");
