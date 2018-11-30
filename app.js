const server = require("./utils/server").server;
// globals
const PORT = process.env.PORT || 2345;

// bootstrap the server

loadIndex();

// not express :-)
const app = server();

app.get("/suggestions", (req, res) => {
  return Promise.resolve(1).then(ok => {
    res.end("hello");
  });
});

app.listen(PORT, "0.0.0.0", () => {
  console.log("server listening in ", PORT);
});
