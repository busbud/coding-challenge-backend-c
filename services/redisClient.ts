import { createClient } from 'redis';
import dotenv from 'dotenv';
dotenv.config();

const REDIS_PORT = process.env.REDIS_PORT_ID as string;

export const redisClient = createClient({
    password: process.env.REDIS_PASSWORD_ID,
    socket: {
        host: process.env.REDIS_HOST_ID,
        port: parseInt(process.env.REDIS_PORT_ID as string)
    }
});
redisClient.on('error', err => console.log('Redis Client Error', err));


 