const http = require("http");
const url = require("url");
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

/** match the basepath of a uri */
function findHandler(uri, handlers) {
  for (let key of Object.keys(handlers)) {
    const handler = handlers[key];
    const parsed = url.parse(uri);
    if (parsed.pathname === key) return handler;
  }
  return null;
}

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
          const handler = findHandler(base_url, handlers);
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
