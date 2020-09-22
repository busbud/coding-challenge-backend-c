import logger from '../config/logger.js';

const wrapAsync = (func) => (req, res, next) => {
  func(req, res, next).catch((err) => next(err));
};

// eslint-disable-next-line no-unused-vars
const genericErrorHandler = (err, _req, res, _next) => {
  logger.error(err.stack);
  res.status(err.status || 500).json(err.body);
};

export default {
  wrapAsync,
  genericErrorHandler,
};
