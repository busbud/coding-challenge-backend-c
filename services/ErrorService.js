/* eslint-disable max-classes-per-file */
// Utilities
const ApiError = require('../utilties/ApiError');

class MissingRequestParameter extends ApiError {
  constructor(parameterName) {
    let message = 'Mandatory request parameter is missing';

    message = parameterName ? `${message}: '${parameterName}'` : message;

    super(message, 400, 'C-1010');
    this.name = this.constructor.name;
  }
}

class ForbiddenError extends ApiError {
  constructor() {
    super(null, 403, 'C-2001');
    this.name = this.constructor.name;
  }
}

class PgError extends ApiError {
  constructor(err) {
    const message = `Error occured while accessing the database :: ${err}`;

    // TODO Log `err` separately.

    super(message, 500, 'C-5000');
    this.name = this.constructor.name;
  }
}

module.exports = {
  MissingRequestParameter,
  ForbiddenError,
  PgError,
};
