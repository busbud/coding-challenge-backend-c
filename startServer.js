//Starts the autocomplete API server
var http = require('http');
var url=require('url');
var _=require('lodash');
var toobusy=require('toobusy-js');

var API_PATH='/suggestions';

exports.go = function (port,responder,search_structure,done) {
    toobusy.maxLag(200);
    var server=http.createServer(function (req, res) {
        if (toobusy()) {
            res.writeHead(503);
            return res.end('Server is too busy.');
        }
        var url_obj=url.parse(req.url,true);
        if (req.method!=='GET'||url_obj.pathname!==API_PATH) {
            res.writeHead(404);
        	return res.end();
        }
        var response_obj=responder.getFormattedResponse(url_obj,search_structure);
        var status_code;
        if (!_.isEmpty(response_obj.suggestions)) {
        	status_code=200;
        }
        else {
        	status_code=404;
        }
        res.writeHead(status_code,{'Content-Type':'application/json; charset=UTF-8'});
        res.end(JSON.stringify(response_obj));
    }).listen(port, '127.0.0.1');
    
    console.log('Server running at http://127.0.0.1:%d/suggestions', port);
    done(null,server);
};