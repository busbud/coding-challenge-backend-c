"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const server_1 = require("./server");
const constants_1 = require("../constants");
(0, server_1.createServer)({
    windowMs: constants_1.MAX_TIME_PER_WINDOW,
    max: constants_1.MAX_SERVER_REQUESTS_PER_WINDOW,
    message: 'Too many requests from this IP. Please wait and try again later.',
    statusCode: 429,
}, true).listen(4000, () => console.log(`Listening on port ${4000}`));
