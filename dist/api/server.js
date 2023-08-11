"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.createServer = void 0;
const express_1 = __importStar(require("express"));
const morgan_1 = __importDefault(require("morgan"));
const router_1 = require("./router");
const express_rate_limit_1 = require("express-rate-limit");
const createServer = (limiterOptions, morganBool) => {
    const app = (0, express_1.default)();
    // mitigation to handle high levels of traffic
    const limiter = (0, express_rate_limit_1.rateLimit)(limiterOptions);
    app.use(limiter);
    const routes = (0, express_1.Router)();
    routes.use('/suggest', router_1.suggestionsRouter);
    app.use(express_1.default.json()).use(express_1.default.urlencoded({ extended: false }));
    if (morganBool) {
        app.use((0, morgan_1.default)('tiny'));
    }
    app.get('/', (req, res) => {
        res.json({ message: 'Welcome, you made it!' });
    }).use('/suggestions', router_1.suggestionsRouter);
    return app;
};
exports.createServer = createServer;
