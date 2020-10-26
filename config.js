const environment = process.env.NODE_ENV ?? 'development'

const DEBUG = environment === 'development'

module.exports = {
  DEBUG,
}