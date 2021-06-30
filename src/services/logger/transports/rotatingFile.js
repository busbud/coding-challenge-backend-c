const path = require('path')
const rfs = require('rotating-file-stream')
const { path: rootPath } = require('app-root-path')

/**
 * @function
 * @description Build the rotating file transports
 * @param {Object} [filename='application.log']
 * @param {string} [size='10M']
 * @param {number} [interval='1d']
 * @param {number} [compress='gzip']
 * @returns {RotatingFileStream}
 */
module.exports = ({
  filename = 'application.log',
  size = '10M',
  interval = '1d',
  compress = 'gzip'
} = {}) =>
  rfs.createStream(path.resolve(rootPath, 'logs', filename), {
    size,
    interval,
    compress
  })
