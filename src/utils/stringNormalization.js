/**
 * Normalize string to remove accent, capital letters, etc...
 * @param {string} s String to normalize
 */
module.exports.normalize = s => s.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase();
