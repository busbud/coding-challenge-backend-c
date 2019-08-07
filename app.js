const express = require("express");
const app = express();
const sass = require("node-sass-middleware");
app.set("view engine", "ejs");
app.use(
  "/styles",
  sass({
    src: __dirname + "/styles",
    dest: __dirname + "/public/styles",
    debug: true,
    outputStyle: "expanded"
  })
);
app.use(express.static(__dirname + "/public"));
PORT = 3000;



// module.exports = http.createServer(function (req, res) {
//   res.writeHead(404, {'Content-Type': 'text/plain'});

//   if (req.url.indexOf('/suggestions') === 0) {
//     res.end(JSON.stringify({
//       suggestions: []
//     }));
//   } else {
//     res.end();
//   }
// }).listen(port, '127.0.0.1');



app.get("/", (req, res) => {
  res.send("hello! Please go to /suggestions");
});

app.get("/suggestions", (req, res) => {
  res.render("index");
});


app.listen(PORT, () => {
  console.log('Server running at localhost:%d/suggestions', PORT);
});


module.exports = app;