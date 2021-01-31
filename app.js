const logger = require('morgan');
const express = require('express');
const createError = require('http-errors');
const cookieParser = require('cookie-parser');

const metrics = require('./src/metrics/requestMetrics');

const SugestionsRouter = require('./src/router/suggestionRouter');
const ActuatorRouter = require('./src/router/actuatorRouter');

class App {
    get app() {
        return this._app;
    }

    get actuator() {
        return this._actuator;
    }

    constructor() {
        this._app = express();

        // The morgan logger app logs request calls
        if (process.env.ENV !== 'production') {
            this.app.use(logger('dev'));
        }
        this.app.use(express.json());
        this.app.use(express.urlencoded({ extended: false }));
        this.app.use(cookieParser());

        this.app.use(metrics.countRequests);

        this.app.use('/suggestions', SugestionsRouter);

        // catch 404 and forward to error handler
        this.app.use((req, res, next) => {
            next(createError(404));
        });

        // error handler
        this.app.use(App.errorHandler);

        // metrics
        this._actuator = express();
        this.actuator.use(express.json());
        this.actuator.use(express.urlencoded({ extended: false }));
        this.actuator.use('/actuator', ActuatorRouter);

        // catch 404 and forward to error handler
        this.actuator.use((req, res, next) => {
            next(createError(404));
        });
        this.actuator.use(App.errorHandler);
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
