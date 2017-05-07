'use strict';

module.exports = {
    db: {
        uri: process.env.MONGOLAB_URI,
        cacheUri: process.env.MONGOLAB_CACHE_URI,
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