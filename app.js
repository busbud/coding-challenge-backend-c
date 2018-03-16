const http = require('http');
const url = require('url');
const port = process.env.PORT || 2345;
const serverHelper = require('./lib/server-helpers');
const util = new serverHelper.util();
const contentType = {'Content-Type': 'application/json'}

module.exports = http.createServer(async (req, res) => {

    // console.log(Date.now(),'Got Request', url.parse(req.url,true))

    if (req.url.indexOf('/suggestions') === 0) {

        try {

            await util.thottleConnection(req);
            let suggestions = await util.getSuggestionsFromRequest(url.parse(req.url, true).query)

            if (suggestions.length)
                res.writeHead(200, contentType);
            else
                res.writeHead(404, contentType);

            res.end(JSON.stringify({
                suggestions
            }));

        } catch (err) {

            res.writeHead(400)
            res.end(JSON.stringify(err));
        }

    } else {
        res.writeHead(404, {'Content-Type': 'text/plain'});
        res.end();
    }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);