'use strict';
/**
 * Module dependencies.
 */

var _ = require('lodash');

/**
* Extends the response object
*
* @param {Object} req
* @param {Object} res
* @callback next callback
*/

module.exports = function(req, res, next) {

    /**
    * Sends a stringified response
    *
    * @param {Object} data - data to stringify
    */

    res.apiResponse = function (data) {
			res.json(data);
		};

    /**
    * Sends an api success response
    *
    * @param {Object[]} [result] - response data
    * @param {string|string[]} [message] - response message(s)
    * @param {number} [code] - response status code
    */

    res.apiSuccess = function(result, messages, code) {
        res.status(code || 200);
        return res.apiResponse({
            status: {
                success: true,
                messages: typeof messages === 'string' ? [messages] : (messages || undefined)
            },
            suggestions: result || []
        });
    };

    /**
    * Sends an api error response
    *
    * @param {string} [key] - error name
    * @param {Object} [err] - error object
    * @param {string|string[]} [message] - response message(s)
    * @param {number} [code=500] - status code
    */

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

    /**
    * Sends an api not found response
    */

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