
import rateLimit from 'express-rate-limit';

export const trafficLimit = rateLimit({
    windowMs: 10000,
    max: 20,
    message: 'Too many requests from this IP, please try again after 10 seconds'
});

