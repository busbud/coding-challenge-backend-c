const http = require("http");
const { curry } = require("./functions");

/** utilities to emulate an express like environement */

/** statuses code */
const endWithCode = curry((code, res, body) => {
  res.writeHead(code, JSON.stringify(body));
  res.end();
});

/** preconfigudre */
const success = endWithCode(200);
const error = endWithCode(500);
const notFound = endWithCode(404);

/** a very simple and thin abstraction over native nodejs api */
// we know this is an api, so no need to implement everything here
function server() {
  const handlers = {};
  return {
    get(url, handler) {
      handlers[url] = handler;
    },
    listen(port, address = "localhost", cb) {
      http
        .createServer((req, res) => {
          const base_url = req.url;
          // TODO: add support for dynamic parameters
          const handler = handlers[base_url];
          if (handler) {
            //TODO: support promise natively
            handler(req, res);
          } else notFound(res, null);
        })
        .listen(port, address);
      if (cb) cb();
    }
  };
}

module.exports = {
  server,
  error,
  notFound,
  success
};
