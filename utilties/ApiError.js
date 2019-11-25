const { STATUS_CODES } = require('http');

class ApiError extends Error {
  constructor(message, status, code) {
    super(message);
    this.name = this.constructor.name;
    this.status = STATUS_CODES[status] ? Number(status) : 500;
    this.code = code || 'DI-0001';
    this.detail = message || 'Something went wrong.';
  }
}

module.exports = ApiError;
