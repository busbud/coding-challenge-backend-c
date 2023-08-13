"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.createServer = void 0;
const express_1 = __importDefault(require("express"));
const morgan_1 = __importDefault(require("morgan"));
const router_1 = require("./router");
const express_rate_limit_1 = require("express-rate-limit");
const createServer = (limiterOptions, morganBool) => {
    const app = (0, express_1.default)();
    const limiter = (0, express_rate_limit_1.rateLimit)(limiterOptions);
    app.use(limiter);
    if (morganBool) {
        app.use((0, morgan_1.default)('tiny'));
    }
    app.get('/', (req, res) => {
        res.json({ message: 'Welcome, you made it!' });
    })
        .use('/suggestions', router_1.suggestionsRouter);
    return app;
};
exports.createServer = createServer;
