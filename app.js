require("module-alias/register");
const express = require("express");
var stackTrace = require("stack-trace");
require("dotenv").config();
const app = express();
const { PORT, ENV_NAME } = process.env;
const port = PORT || 2345;

// Parse JSON bodies (as sent by API clients)
app.use(express.json({ limit: "1gb" }));

// Add cors
app.use(function (req, res, next) {
  res.header("Access-Control-Allow-Origin", `*`); //* will allow from all cross domain
  res.header(
    "Access-Control-Allow-Headers",
    "Origin, X-Requested-With, Content-Type, Accept"
  );
  res.header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
  next();
});

app.get("/", (req, res) => {
  return res.send("Healthy");
});

const startup = require("./app/startup");

startup(app);

app.route("*").all(function (req, res, next) {
  next({
    status: 404,
    message: "The route you are trying to get is not defined",
  });
});

// error handler middleware
app.use((error, req, res, next) => {
  const stack = stackTrace.parse(error);

  console.log({
    error,
    endpoint: req.url,
    ...(Array.isArray(stack) &&
      stack[0] && {
        fileName: stack[0].getFileName(),
        lineNumber: stack[0].getLineNumber(),
      }),
  });

  // if its an internal error and its not running on local
  // then send a general error message
  // if (ENV_NAME !== "development")
  //   return res.status(500).json({
  //     type: "InternalServerError",
  //     code: "errors.internal",
  //     message: "Internal Server Error",
  //   });

  return res.status(error.status || 500).json({
    type: error.type || "InternalServerError",
    code: error.code || "errors.internal",
    message: error.message || "Internal Server Error",
    ...(error.payload && error.payload),
  });
});

app.listen(port, () => console.log(`Server running on port ${port}!`));

module.exports = app;
// const express = require("express");
// const app = express();
// var port = process.env.PORT || 2345;
// const startup = require("./app/startup");

// startup(app);

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

// console.log('Server running at http://127.0.0.1:%d/suggestions', port);
