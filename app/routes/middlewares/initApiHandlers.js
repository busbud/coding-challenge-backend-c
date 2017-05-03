'use strict';
/**
 * initApiHandlers.js
 * ------------------------------
 * Inits the API handler functions
 */
var _ = require('lodash');

module.exports = function(req, res, next) {
    res.apiResponse = function (data) {
			res.json(data);
		};

    res.apiSuccess = function(result, messages, code) {
        res.status(code || 200);
        return res.apiResponse({
            status: {
                success: true,
                messages: typeof messages === 'string' ? [messages] : (messages || undefined)
            },
            suggestions: result
        });
    };

    res.apiError = function(key, err, messages, code) {
        key = key || err.name || 'Unknown error';
        messages = messages || (err.errors ? _.map(err.errors, 'message') : (err.message || 'Unknown error'));
        res.status(code || 500);
        return res.apiResponse({
            error: key,
            status: {
                success: false,
                messages: typeof messages === 'string' ? [messages] : (messages || undefined)
            },
            suggestions: []
        });
    };

    res.apiNotFound = function() {
        res.status(404);
        return res.apiResponse({
            error: 'NotFound',
            status: {
                success: false,
                messages: ['API not found']
            }
        });
    };

    return next();
};