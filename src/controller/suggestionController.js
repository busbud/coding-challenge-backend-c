const SuggestionService = require('../service/suggestionService');

const MIN_LATITUDE_VALUE = -90;
const MAX_LATITUDE_VALUE = 90;

const MIN_LONGITUDE_VALUE = -180;
const MAX_LONGITUDE_VALUE = 180;

const outOfRangeMessage = (value, min, max) => `${value} range from ${min} to ${max}.`;

const isOutOfRange = (value, min, max) => value < min || value > max;

const validateQuery = (query) => {
    if (!query) {
        return {
            code: 400,
            message: 'Query param cannot be empty.',
        };
    }

    // TODO: should we allow queries with lenght greater than X?
    // No city has a name bigger than

    return null;
};

const validateCoordinates = (latitude, longitude) => {
    // TODO: should we allow a single coordinate to be passed? Or should we return bad a request?
    if ((!latitude && longitude) || (latitude && !longitude)) {
        return {
            code: 400,
            message: 'Both latitude and longitude values should be provided to use coordinates.',
        };
    }

    if (isOutOfRange(latitude, MIN_LATITUDE_VALUE, MAX_LATITUDE_VALUE)) {
        return {
            code: 400,
            message: outOfRangeMessage('Latitudes', MIN_LATITUDE_VALUE, MAX_LATITUDE_VALUE),
        };
    }

    if (isOutOfRange(longitude, MIN_LONGITUDE_VALUE, MAX_LONGITUDE_VALUE)) {
        return {
            code: 400,
            message: outOfRangeMessage('Longitudes', MIN_LONGITUDE_VALUE, MAX_LONGITUDE_VALUE),
        };
    }

    return null;
};

class SuggestionController {
    static async getSuggestions(request, response) {
        const { q: query, latitude, longitude } = request.query;

        const queryError = validateQuery(query);
        if (queryError) {
            return response.status(queryError.code).send(queryError);
        }

        const coordinatesError = validateCoordinates(latitude, longitude);
        if (coordinatesError) {
            return response.status(coordinatesError.code).send(coordinatesError);
        }

        const suggestions = SuggestionService.getSuggestions(query, latitude, longitude);

        if (suggestions.length === 0) {
            return response.status(404).send({ suggestions });
        }

        return response.json({ suggestions });
    }
}

module.exports = SuggestionController;
