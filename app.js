var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;

function parse_data(filename) {

  var fs = require('fs');
  var es = require('event-stream');

  canada_states = ['AB', 'BC', 'MB', 'NB', 'NL', 'NS', 'NT', 'NU', 'ON', 'PE', 'QC', 'SK', 'YT'];

  parsed_data = [];

  fs.createReadStream(filename).pipe(es.split('\n')).pipe(es.mapSync(function(data) {
    row = data.split('\t');

    if(row[14]>5000 && (row[8]=='US' || row[8]=='CA')) {

      state = row[10];
      if(row[8]=='CA') state = canada_states[parseInt(state)];

      parsed_data.push({name:row[2]+', '+state+', '+row[8], latitude:row[4], longitude:row[5]});
    }

  }));

  return parsed_data;
}

function position_penalty(data, search) {
  return Math.sqrt(Math.abs((data-search)/data));
}

function find_suggestions(data, name, latitude, longitude) {

  if(name == false && latitude == false && longitude == false) return "";

  results = [];
  for (index = 0; index < data.length; ++index) {

    score = 1;

    if(name) {
      if(data[index].name.indexOf(name) != 0) score = 0;
      else score = score - Math.pow((data[index].name.length-name.length)/data[index].name.length,8);
    }

    if(latitude) {
      score = score - position_penalty(data[index].latitude, latitude);
    }

    if(longitude) {
      score = score - position_penalty(data[index].longitude, longitude);
    }

    if(score<0) score = 0;

    // round to 1 decimal
    data[index].score = Math.round(score*10)/10;

    // add to results if higher than 0.2
    if(data[index].score>=0.2) results.push(data[index]);
  }

  // sort by score in descending order
  results.sort(function(a, b){return b.score-a.score});

  return results;
}

var data = parse_data('data/cities_canada-usa.tsv');

String.prototype.test = function () {
  return find_suggestions(data,this,false,false);
};

module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {

    var query = url.parse(req.url,true).query;
    result = find_suggestions(data,query.q,query.latitude,query.longitude);

    if(result.length) status = 200;
    else status = 404;

    res.writeHead(status, {'Content-Type': 'application/json'});
    res.end(JSON.stringify({
      suggestions: result
    }));

  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);