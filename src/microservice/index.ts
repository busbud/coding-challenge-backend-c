import * as bodyParser from 'body-parser';
import RateLimit from 'express-rate-limit';

import {middleware as cache} from 'apicache';

import logger from '../middlewares/logger';
import {errorHandler} from '../middlewares/error';

// Routes
import suggestionsEndpoint from './routes/suggestions';

// Caching
let cachingForSuggestions = cache('5 minutes');
cachingForSuggestions.options = {
    statusCode: {include: [200]},
    appendKey: (req: Express.Request & { query: any }): string => {
        const {q, latitude, longitude} = req.query;
        return `${q}-${latitude}-${longitude}`.toLowerCase(); // Cache Key
    }
};

// Health endpoint for load balancing
const healthEndpoint = (req: any ,res: any): void => {
    res.send({code: 200});
};

const routesDefinition: { [key: string]: { [key: string]: any}  } = {
    'GET': {
        '/suggestions': [cachingForSuggestions,suggestionsEndpoint],
        '/health': [healthEndpoint],
    }
};

const loggerMiddleware = logger();

const limiter = new RateLimit({
    windowMs: 15 * 60 * 1000,
    max: 200,
    onLimitReached: (req, res, options: RateLimit.Options): void => {
        // Log, mail, whatever needed.
        res.status(429).send(options.message);
    }
});

const middlewares = [
    limiter,
    bodyParser.json(),
    bodyParser.urlencoded({extended: true}),
    loggerMiddleware,
    errorHandler
];

/**
 * Binds routes & middlewares for given app parameter
 * @param routesDefinition Routes to be partially applied
 * @param middlewares Middlewares to be partially applied
 */
export const applyMicroservicesRoutes = (routesDefinition: any, middlewares: any): any => (app: any): void => {

    // For Heroku, AWS ELB, Nginx.
    // Takes latest X-Forwarded-For header instead of req.connection.remoteAddress
    app.enable('trust proxy');

    // Apply the middlewares
    middlewares.forEach((middleware: any): void => app.use(middleware));

    // Apply the routes
    Object.keys(routesDefinition).forEach((method): void => {
        const routes: string[] = Object.keys(routesDefinition[method]);
        Object.keys(routes).forEach((key: any): void => {
            const route = routes[key];
            const fns = routesDefinition[method][route];
            app[method.toLowerCase()](route,...fns);
            console.log(`Setting [up] [${method}] ${route}`);
        });
    });
};

export default applyMicroservicesRoutes(routesDefinition, middlewares);
