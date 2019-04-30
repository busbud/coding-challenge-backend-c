// Define console.log as the default logger
const loggerFunction = (...args: any[]) => console.log(...args);


/**
 * @param options {Object} Configurations options
 * @param options.logLevel {Number} Verbosity
 * @param options.logger {Function} Default logger
 */
interface LoggerOptions {
    loggerFunction?: any;
    logLevel?: number;
    minLogLevel?: number;
}
const defaultLoggerOptions = {
    loggerFunction,
    logLevel: 4,
    minLogLevel: 0,
};

// Shorthand for lodash _.default
export const defaults = (received: LoggerOptions, deflt: LoggerOptions): LoggerOptions => Object.assign({},deflt,received);

/**
 * Returns an express middleware function to be used to manage logging
 * @param options Options object
 */
export const logger = (options={}): any => {
    const {loggerFunction, logLevel} = defaults(options,defaultLoggerOptions);
    /**
     * @param req {Object} Express Request
     * @param res {Object} Express Response
     * @param next {Function} Callback to next middleware
     */
    const loggerMiddleware = (req: any, res: any, next: any): void => {
        // Timestamp the request start to benchmark the speed after all middlewares resolve.
        req.requestTime = Date.now();
        loggerFunction(`${req.url} - ${req.requestTime} - [${req.headers['user-agent']}] - ${req.headers['x-forwarded-for'] || req.connection.remoteAddress}`, {logLevel});
        next();
    };
    return loggerMiddleware;
};

export default logger;
