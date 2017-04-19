'use strict'

module.exports = {
    name: 'BUSBUDEMI API',
    version: '1.0.0',
    env: process.env.NODE_ENV || 'development',
    port: process.env.PORT || 2345,
    base_url: process.env.BASE_URL || 'http://localhost:2345',
    db: {
        uri: 'mongodb://127.0.0.1:27017/busbudemi_api',
        options: {
        	user: 'busbudemi',
  			pass: 'alamakota217'
        }
    },
}
