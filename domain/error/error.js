class ValidationError extends Error {
    constructor(message) {
        super(message);
        this.name = "ValidationError";
    }
}

class InternalServerError extends Error {
    constructor(message) {
        super(message);
        this.name = "InternalServerError";
    }
}

class NotFoundError extends Error {
    constructor(message) {
        super(message);
        this.name = "NotFound";
    }
}

module.exports.ValidationError = ValidationError;
module.exports.InternalServerError = InternalServerError;
module.exports.NotFoundError = NotFoundError;
