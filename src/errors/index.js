const { inherits } = require('util')
const status = require('statuses')
const toIdentifier = require('toidentifier')

function formatName (name, appendError = true) {
  if (appendError) {
    return `${toIdentifier(name)}Error`
  }
  return toIdentifier(name)
}

/**
 * Errors
 * @module errors
 */

/**
 * @function createError
 * @description Create an HTTP error with the provided information
 * @param {number} statusCode - The status code.
 * @param {string} name - The error's name.
 * @param {object} options
 * @param {boolean} [options.expose=true] - Can be used to signal if message should be sent to the client.
 * @returns {HTTPError}
 */
function createError (
  statusCode, name, { expose = true }) {
  if (!statusCode) throw new Error('HTTP error status code must not be empty')

  function HTTPError (message, { info = {}, cause = null } = {}) {
    if (!(this instanceof HTTPError)) {
      return new HTTPError(message, { info, cause })
    }

    this.name = name
    this.message = message
    this.isHTTPError = true
    this.statusCode = statusCode
    this.expose = expose
    this.info = info

    Error.captureStackTrace(this, this.constructor)
    if (cause) {
      this.stack = cause.stack + '\n' +
        this.stack
    }
  }

  inherits(HTTPError, Error)

  HTTPError.prototype[Symbol.toStringTag] = 'Error'

  HTTPError.prototype.toString = function () {
    return `${this.name}: ${this.message}`
  }

  HTTPError.prototype.payload = function () {
    return {
      name: this.name,
      message: this.expose
        ? this.message
        : 'An error occurred; contact technical support',
      info: this.info
    }
  }

  return HTTPError
}

for (const statusCode of status.codes) {
  const codeClass = parseInt(statusCode.toString().charAt(0).padEnd(3, '0'))
  let name
  if (statusCode === 500) {
    name = formatName(status(statusCode), false)
  } else {
    name = formatName(status(statusCode))
  }

  switch (codeClass) {
    case 400:
      module.exports[name] = createError(statusCode, name, {})
      break
    case 500:
      module.exports[name] = createError(statusCode, name, { expose: false })
      break
  }
}

module.exports.ValidationError = createError(400, 'ValidationError',
  { expose: true })
