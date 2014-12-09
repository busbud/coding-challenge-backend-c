//Takes flat collection of city objects and generates searchable data structure
var parser=require('./citiesParser'); //Maybe no parsing here
var matches=require('./matches');
var Trie=require('./trie');

exports.makeStructure = function(cities_flat,done) {//Makes structure and passes it as argument to done(err, search_structure)
	var search_struct = {};
	search_struct.struct=buildStructure(cities_flat);  
	search_struct.getMatches = function(search_term) {
		return matches.getMatches(this.struct,search_term);
	};
	console.log('Built cities trie.')
	done(null,search_struct);
};

function buildStructure(cities_flat) {//Build trie from flat collection of cities
	var trie=new Trie();
	for (var key in cities_flat) {
		city=cities_flat[key];
		addCity(trie,city);
	}	
	return trie;
}
function addCity(trie,city) {//Add city_hits to trie for city
	var primary_hit = {
		city : city,
		hit_type : 'primary' //Better to use enum approach?
	};
	trie.addEntry(city.primary_name.toLowerCase(),primary_hit);
	if (city.ascii_name!==city.primary_name) {
		var ascii_hit = {
			city : city,
			hit_type : 'ascii'
		};
		trie.addEntry(city.ascii_name.toLowerCase(),ascii_hit);
	}
	for (var key in city.alt_names) {
		var alt_name=city.alt_names[key];
		if (alt_name!==city.primary_name) {
			var alt_hit = {
				city : city,
				hit_type : 'alt'
			};
			trie.addEntry(alt_name.toLowerCase(),alt_hit);
		}
	}
}