// const express = require("express");
// const app = express();
// const sass = require("node-sass-middleware");
// app.set("view engine", "ejs");
// app.use(
//   "/styles",
//   sass({
//     src: __dirname + "/styles",
//     dest: __dirname + "/public/styles",
//     debug: true,
//     outputStyle: "expanded"
//   })
// );
// app.use(express.static(__dirname + "/public"));
// PORT = 3000;




// app.get("/", (req, res) => {
//   res.send("hello! Please go to /suggestions");
// });


// app.listen(PORT, () => {
//   console.log('Server running at localhost:%d/suggestions', PORT);
// });


// module.exports = app;

////////////////////////////////////////////////////////////////////////////
const http = require('http');
const port = process.env.PORT || 2345;
const fs = require('fs');

fs.readFile('./data/cities_canada-usa.tsv', 'utf-8', function (err, data) {
    if (err) {
        throw err;
    }
    const content = data.toString();
    console.log(content)
});


module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);