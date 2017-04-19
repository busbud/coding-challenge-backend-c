'use strict'

module.exports = {
    name: 'BUSBUDEMI API',
    version: '1.0.0',
    env: process.env.NODE_ENV || 'development',
    port: process.env.PORT || 2345,
    base_url: process.env.BASE_URL || 'http://localhost:2345',
    db: {
        uri: 'mongodb://ds111441.mlab.com:11441/heroku_w5b0rm2r',
        options: {
        	user: 'busbudemi',
  			pass: 'alamakota217'
        }
    },
}
