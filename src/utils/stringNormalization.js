module.exports.normalize = 
/**
 * Normalize string to remove accent, capital letters, etc...
 * @param {string} s String to normalize
 */
function (s) {
    return s.normalize('NFD').replace(/[\u0300-\u036f]/g, "").toLowerCase();
}