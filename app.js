var async=require('async');
var server=require('./startServer');
var responder=require('./responder');
var parser=require('./citiesParser');
var struct_builder=require('./searchStructure');

var CITIES_FILE='./data/cities_canada-usa.tsv';
var port = process.env.PORT || 2345;

module.exports = main();

function main() {
	//Parse cities
	//Build search structure
	//Launch server
	// server.go(port,responder);
	async.waterfall([
		function (step) {
			parser.getCities(CITIES_FILE,step);
		},
		function (cities_flat,step) {
			struct_builder.makeStructure(cities_flat,step);
		},
		function (search_structure,step) {
			server.go(port,responder,search_structure);
		}
		]);
}
