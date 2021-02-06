const logger = require('morgan');
const express = require('express');
const createError = require('http-errors');
const cookieParser = require('cookie-parser');
const swaggerUi = require('swagger-ui-express');
const cors = require('cors');

const metrics = require('./src/metrics/requestMetrics');

const SugestionsRouter = require('./src/router/suggestionRouter');
const ActuatorRouter = require('./src/router/actuatorRouter');

const { swaggerSpecification } = require('./src/swagger');

class App {
    get appListener() {
        return this._app;
    }

    get actuatorListener() {
        return this._actuator;
    }

    constructor() {
        this.configureApp();

        this.configureActuator();
    }

    configureApp() {
        this._app = express();

        // The morgan logger app logs request calls
        if (process.env.ENV !== 'production') {
            this.appListener.use(logger('dev'));
        }
        this.appListener.use(express.json());
        this.appListener.use(express.urlencoded({ extended: false }));
        this.appListener.use(cookieParser());

        this.appListener.use(metrics.countRequests);

        this.appListener.use(cors());
        this.appListener.use('/suggestions', SugestionsRouter);
        this.appListener.use('/api-docs', swaggerUi.serve, swaggerUi.setup(swaggerSpecification));

        // catch 404 and forward to error handler
        this.appListener.use((req, res, next) => {
            next(createError(404));
        });

        // error handler
        this.appListener.use(App.errorHandler);
    }

    configureActuator() {
        // metrics
        this._actuator = express();
        this.actuatorListener.use(express.json());
        this.actuatorListener.use(express.urlencoded({ extended: false }));
        this.actuatorListener.use('/actuator', ActuatorRouter);

        // catch 404 and forward to error handler
        this.actuatorListener.use((req, res, next) => {
            next(createError(404));
        });
        this.actuatorListener.use(App.errorHandler);
    }

    static errorHandler(err, req, res) {
        // set locals, only providing error in development
        res.locals.message = err.message;
        res.locals.error = req.app.get('env') === 'development' ? err : {};

        // render the error page
        return res.status(err.status || 500);
    }
}

module.exports = App;
