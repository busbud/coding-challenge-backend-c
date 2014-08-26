var fs = require('fs');
var csv = require('csv');

// Mapping from: http://download.geonames.org/export/dump/admin1CodesASCII.txt
var canadaStateCodes = {
  1: 'AB',
  2: 'BC',
  3: 'MB',
  4: 'NB',
  13: 'NT',
  07: 'NS',
  14: 'NU,',
  08: 'ON',
  09: 'PE',
  10: 'QC',
  11: 'SK',
  12: 'YT',
  05: 'NL'
};

exports.toJson = function(file, callback) {
  var output = [];

  if(!fs.existsSync(file)) {
    console.log('File not found');
    return;
  }
  var input = fs.createReadStream(file);

  var parser = csv.parse({columns: true, delimiter: '\t', quote: '\"\"', escape: '\\'});
  var transformer = csv.transform(function(data) {
    if(data['admin1'] != undefined && data['country'] == 'CA') {
      data['admin1'] = canadaStateCodes[data['admin1']];
    }

    return {
      id: data['id'],
      name: data['name'],
      state: data['admin1'],
      country: data['country'],
      latitude: data['lat'],
      longitude: data['long'],
    };
  });

  parser.on('readable', function() {
    while(data = parser.read()) {
      transformer.write(data);
    }
  });

  parser.on('error', function(err) {
    console.log(err.message);
  });

  transformer.on('readable', function() {
    while(data = transformer.read()) {
      output.push(data)
    }
  });

  // Execute
  input.pipe(parser);

  // Call callback function
  parser.on('finish', function() {
    callback(output);
  });
}
