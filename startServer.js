//Starts the autocomplete API server
var http = require('http');
var url=require('url');

var API_PATH='/suggestions';

exports.go = function (port,responder,search_structure,done) {
    var server=http.createServer(function (req, res) {
        // res.writeHead(200, { 'Content-Type': 'application/json' });
        
        // if (req.url.indexOf('/suggestions') === 0) {
        //     res.end(responder.getResponseString(url.parse(req.url,true),search_structure));
        // } else {
        //     res.end();
        // }
        var url_obj=url.parse(req.url,true);
        if (req.method!=='GET'||url_obj.pathname!==API_PATH) {
        	res.end();
        	return;
        }
        var response_obj=responder.getFormattedResponse(url_obj,search_structure);
        var status_code;
        // console.log(response_obj.suggestions.length);
        if (response_obj.suggestions.length) {
        	status_code=200;
        }
        else {
        	status_code=404;
        }
        // console.log(status_code);
        res.writeHead(status_code,{'Content-Type':'application/json'});
        res.end(JSON.stringify(response_obj));
    }).listen(port, '127.0.0.1');
    
    console.log('Server running at http://127.0.0.1:%d/suggestions', port);
    done(null,server);
};