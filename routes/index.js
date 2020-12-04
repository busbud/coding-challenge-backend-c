const express = require('express');
const boom = require('boom');

const MIN_VALUE_LONGITUDE = -180;
const MAX_VALUE_LONGITUDE = 180;

const MIN_VALUE_LATITUDE = -90;
const MAX_VALUE_LATITUDE = 90;

module.exports = (app, service, client) => {
	const router = express.Router();
	app.use('/v0', router);
	router.get(
		'/suggestions',
		async (req, res, next) => {
			const { q, latitude, longitude } = req.query;
			try {
				if (!q) {
					throw boom.badRequest('Query parameter is required.');
				}
				if (
					longitude
					&& (Number(longitude) <= MIN_VALUE_LONGITUDE || Number(longitude) >= MAX_VALUE_LONGITUDE)
				) {
					throw boom.badRequest('Bad value for longitude, it should be between -180 to 180', longitude);
				}
				if (
					latitude
					&& (Number(latitude) <= MIN_VALUE_LATITUDE || Number(latitude) >= MAX_VALUE_LATITUDE)
				) {
					throw boom.badRequest('Bad value for latitude, it should be between -90 to 90', latitude);
				}
				const suggestions = await service.suggestCities(
					q, latitude, longitude, client,
				);
				return res.status(200).json({
					suggestions,
				});
			} catch (err) {
				app.log.error('GET suggestions error', err);
				return next(err);
			}
		},
	);
};
