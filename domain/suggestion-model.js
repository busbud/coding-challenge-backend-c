class Suggestion {
    constructor(name, latitude, longitude, score) {
        this.name = name;
        this.latitude = latitude;
        this.longitude = longitude;
        this.score = score;
    }
}

/**
 * 
 * @param {string} name 
 * @param {number} latitude 
 * @param {number} longitude 
 * @param {number} score 
 */
module.exports.makeSuggestion = (name, latitude, longitude, score) => {
    return new Suggestion(name, latitude, longitude, score);
}