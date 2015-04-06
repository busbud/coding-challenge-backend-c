var diacritics = require('diacritics');

// This function will format a string for Trie insertion and retrieval
// Basically, we want to lowercase everything to make searching case-
// insensitive, as well as strip off all diacritics from our characters
// This is so that is someone searches for 'Montre' or 'montré', 'Montréal' 
// still matches, as it was inserted into the Trie as 'montreal'
function trie_format_string(string) {
    return diacritics.remove(string.toLowerCase());
}

module.exports = {
    trie_format_string: trie_format_string
}