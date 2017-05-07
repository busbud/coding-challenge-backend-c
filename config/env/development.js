'use strict';

module.exports = {
    db: {
        uri: process.env.MONGOLAB_URI,
        options: {
            server: {
                socketOptions: {
                    keepAlive: 1,
                    connectTimeoutMS: 30000
                }
            }
        }
    }
};