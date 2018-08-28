class Logger {
    log(message, extraParams) {
        console.log(message), extraParams;
    }
    error(message, extraParams) {
        console.error(message, extraParams);
    }
}

module.exports = Logger;