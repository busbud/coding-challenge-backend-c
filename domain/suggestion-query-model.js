const { ValidationError } = require('./error/error');

class SuggestionQuery {
    constructor(q, latitude, longitude) {
        this.q = q;
        this.latitude = latitude;
        this.longitude = longitude;
    }
}

/**
 * 
 * @param {string} q 
 * @param {number} latitude 
 * @param {number} longitude 
 */
module.exports.makeSuggestionsQuery = (q, latitude, longitude) => {
    const query = new SuggestionQuery(q, latitude, longitude);

    return validate(query);
}

const validate = (query) => {
    let errors = '';
    
    if (!query.q || query.q.length === 0) {
        errors += 'q is required and cannot be empty';
    }
    if (query.latitude && isNaN(query.latitude)) {
        errors += 'latitude must be a number';
    }
    if (query.longitude && isNaN(query.longitude)) {
        errors += 'longitude must be a number';
    }

    if (errors.length > 0) {
        throw new ValidationError(errors);
    }

    return query;
}