const SuggestionRouter = require('express').Router();

const SuggestionController = require('../controller/suggestionController');

/**
   * @swagger
   * components:
   *   schemas:
   *     Suggestion:
   *       type: object
   *       required:
   *         - name
   *         - latitude
   *         - longitude
   *         - score
   *       properties:
   *         name:
   *           type: string
   *           description: The location name including city, state and country
   *           example: New London, WI, USA
   *         latitude:
   *           type: float
   *           description: The location latitude
   *           example: 44.39276
   *         longitude:
   *           type: float
   *           description: The location longitude
   *           example: -88.73983
   *         score:
   *           type: float
   *           description: The location score for the given query string and possible location passed in the request
   *           example: 1
   */

/**
   * @swagger
   * components:
   *   schemas:
   *     SuggestionResponse:
   *       type: object
   *       required:
   *         - suggestions
   *       properties:
   *         suggestions:
   *         description: A list of suggestions for the given query
   *         type: array
   *         items:
   *           $ref: '#/components/schemas/Suggestion'
   */

/**
   * @swagger
   * components:
   *   schemas:
   *     Error:
   *       type: object
   *       required:
   *         - code
   *         - message
   *       properties:
   *         code:
   *           type: integer
   *           description: The http error code
   *           example: 400
   *         message:
   *           type: string
   *           description: The error message
   *           example: Query param cannot be empty.
   */

/**
 * @swagger
 *
 * /suggestions:
 *   get:
 *     description: This endpoint returns location suggestions for a given query and location
 *     produces:
 *       - application/json
 *     parameters:
 *       - name: q
 *         in: query
 *         required: true
 *         type: string
 *       - name: latitude
 *         in: query
 *         required: false
 *         type: float
 *       - name: longitude
 *         in: query
 *         required: false
 *         type: float
 *     responses:
 *       200:
 *         description: An object containing the list of suggestions for the given query
 *         $ref: '#/components/schemas/SuggestionResponse'
 *       400:
 *         description: Bad request
 *         $ref: '#/components/schemas/Error'
 *       404:
 *         description: No suggestions found
 *         $ref: '#/components/schemas/SuggestionResponse'
 */
SuggestionRouter.get('/', SuggestionController.getSuggestions);

module.exports = SuggestionRouter;
