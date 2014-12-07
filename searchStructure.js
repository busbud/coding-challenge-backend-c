//Takes flat collection of city objects and generates searchable data structure
var parser=require('./citiesParser'); //Maybe no parsing here
var matches=require('./matches');

exports.makeStructure = function(cities_flat,done) {//Makes structure and passes it as argument to done(err, search_structure)
	var search_struct=buildStructure(cities_flat);  //TODO: this should be a holder property on search_struct
	search_struct.getMatches = function(search_term) {
		return matches.getMatches(this,search_term);
	};
	done(null,search_struct);
};

function buildStructure(cities_flat) {
	return cities_flat; //TODO: implement this - trie or something, no?
}