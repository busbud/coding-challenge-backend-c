//Get cities that potentially match a search term

exports.getMatches = function(hit_trie, search_term) {
    return hit_trie.retrievePartialMatches(search_term); //Well then
    //TODO: remove duplicates
};