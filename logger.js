var winston = require('winston');

var winston_transports = [
    new winston.transports.Console({
        json: true,
        colorize: true
    })
]

var logger = new (winston.Logger)({
    transports: winston_transports
});


module.exports = {
    logger: logger,
    transports: winston_transports
}