//Get cities that potentially match a search term
var _=require('lodash');

exports.getMatches = function(hit_trie, search_term) {
	//TODO: use first word in search_term, not whole string
    var matches = hit_trie.retrievePartialMatches(search_term); //Well then
    var primary_matches = _(matches).
    	filter(function(hit) {
    		return hit.hit_type==='primary';
    	}).
    	map(function(hit) {
    		return hit.city;
    	}).
    	value();
    var ascii_matches = _(matches).
    	filter(function(hit) {
    		return hit.hit_type==='ascii';
    	}).
    	map(function(hit) {
    		return hit.city;
    	}).
    	value();
    var unique_matches = _(matches).filter(function(hit) {
    	var dupl_primary=(hit.hit_type==='ascii'||hit.hit_type==='alt')&&
    		_.contains(primary_matches, hit.city); //Contains check could be faster exploiting hashing of JS properties
    	var dupl_ascii=hit.hit_type==='alt'&&
    		_.contains(ascii_matches,hit.city); //Ditto
    	return !(dupl_primary||dupl_ascii);
    }).value();
    // console.log(matches);
    // console.log(unique_matches);
    return matches;
    return unique_matches;
};