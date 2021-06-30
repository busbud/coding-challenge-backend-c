const pino = require('pino')
const { multistream, prettyStream } = require('pino-multi-stream')
const { rotatingFile } = require('./transports')

/**
 * @typedef {Pino} Logger
 * @function
 * @description Build the Pino logger
 * @param {Object} options
 * @param {Object} options.transports
 * @return {Logger}
 */
function buildLogger ({ transports }) {
  const streams = []
  const { rotatingFile: rotatingFileConfig, stdout: stdoutConfig } = transports
  if (rotatingFileConfig && rotatingFileConfig.enabled) {
    streams.push({
      level: rotatingFileConfig.level || 'info',
      stream: rotatingFile(rotatingFileConfig)
    })
  }

  if (stdoutConfig && stdoutConfig.enabled) {
    if (stdoutConfig.pretty) {
      streams.push({
        level: stdoutConfig.level || 'info',
        stream: prettyStream({
          prettyPrint: {
            colorize: true,
            translateTime: 'SYS:standard'
          }
        })
      })
    } else {
      streams.push({ stream: process.stdout })
    }
  }

  return pino({}, multistream(streams))
}

module.exports = buildLogger
