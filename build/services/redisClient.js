"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.redisClient = void 0;
require('dotenv').config();
const redis_1 = require("redis");
exports.redisClient = (0, redis_1.createClient)({
    password: process.env.REDIS_PASSWORD_ID,
    socket: {
        host: process.env.REDIS_HOST_ID,
        port: parseInt(process.env.REDIS_PORT_ID)
    }
});
exports.redisClient.on('error', err => console.log('Redis Client Error', err));
//# sourceMappingURL=redisClient.js.map