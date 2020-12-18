/**
 * 
 * @param {string} errorName 
 */
module.exports.getHttpCode = (errorName) => {
    return dict[errorName] || 500;
}

const dict = {};
dict["ValidationError"] = 400;
