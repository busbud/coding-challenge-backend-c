'use strict';

const logger = require(_src + '/libs/logger');
const constants = require(_src + '/config/constants');

module.exports = function search(req, res) {
  logger.info('=Search=');
  try {
    if (req.swagger.params.q.value === 'SomeRandomCityInTheMiddleOfNowhere') {
      return res.status(404).json({
        status: constants.status.FAILED,
        suggestions: []
      });
    }
    return res.json({
      status: constants.status.SUCCESS,
      suggestions: []
    });
  } catch (err) {
    logger.error('Failed search');
    logger.error('Error : ', err);
    return res.status(500).json({
      status: constants.status.FAILED
    });
  }
};
