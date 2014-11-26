var fs = require('fs');
var stream = require('stream');

var DATA_FILE_PATH = './cities_canada-usa.tsv';
var OUTPUT_PATH = 'data.js';
var MIN_POPULATION = 5000;

console.log(MIN_POPULATION);

fs.unlinkSync(OUTPUT_PATH);

var reader = fs.createReadStream(DATA_FILE_PATH);
var writer = fs.openSync(OUTPUT_PATH, 'w');
var output_buffer = [];

console.log("Cleaning up the data...");

reader.on('data', function(data) {
	var lines = data.toString().split('\n');
	var i, j;
	var city;
	var data_line = "";
	for(i = 0; i<lines.length; i++) {
		city = lines[i].split('\t');
		if(i == 0)
			data_line = 'exports.cities = "';
		else data_line = '"\\n';
		if(city[14] >= MIN_POPULATION) {
			data_line += lines[i];
			data_line += '"+'
			output_buffer.push(data_line);
		}
	}
});

reader.on('end', function() {
	var last_line = output_buffer[output_buffer.length-1];
	last_line = last_line.substring(0,last_line.length-1)+";";
	output_buffer[output_buffer.length-1] = last_line;
	fs.writeSync(writer, output_buffer.join('\n'));
	console.log("Done.");
});