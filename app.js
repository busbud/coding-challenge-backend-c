'use strict';

var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;

var util = require('./lib/util');
var removeDiacritics = require('diacritics').remove;

//Loading database on server start.
var Region = require('./lib/Region');
var usa_ca_region = new Region({});

module.exports = http.createServer(function (req, res) {
  
  if(req.url.indexOf('/suggestions') === 0){

  	var qryObject = url.parse(req.url,true).query;

  	if(qryObject.q != null){
  		
  		//Trim accent and other non ascii char
  		var strFilter = removeDiacritics(qryObject.q);

  		usa_ca_region.getSuggestions(strFilter, qryObject.latitude, qryObject.longitude, function(results){	

  			if (results.suggestions.length > 0){
  				res.writeHead(200, { 'Content-Type': 'application/json' });
  			} else {
  				res.writeHead(404, {'Content-Type': 'application/json'});
  			}
  			res.end(JSON.stringify(results, null, 4));
	  	});

  	} else {
  		var response = {'results':'Syntax error. Wrong arguments'};
	  	res.writeHead(404, {'Content-Type': 'text/json'});
	  	res.end(JSON.stringify(response, null, 4));
  	}
  } else {
  	var response = {'results':'Wrong endpoint. Read the API documentation for more information'};
  	res.writeHead(400, {'Content-Type': 'text/json'});
  	res.end(JSON.stringify(response, null, 4));
  }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port); 