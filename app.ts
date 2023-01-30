import * as http from "http";
const port = process.env.PORT || 2345;

const server = http.createServer(function (req, res) {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    console.log(req.url);
    if (req.url?.indexOf('/suggestions') === 0) {
        res.end(JSON.stringify({
            suggestions: []
        }));
    } else {
        res.end();
    }
});

server.listen(port,() => {console.log(`listening at ${port}`)})