const express = require("express");
const app = express();
const http = require("http").Server(app);
const port = process.env.PORT || 2345;

http.listen(port, () => {
  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
});

app.use((req, res, next) => {
  let query = req.query.q;
  let { longitude, latitude } = req.query;

  if (query === undefined) {
    return res.status(400).send();
  }

  if ((longitude === undefined && latitude !== undefined) || (longitude !== undefined && latitude === undefined)) {
    return res.status(400).send();
  }

  next();
});

app.get("/suggestions", (req, res) => {
  let query = req.query.q;
  let { longitude, latitude } = req.query;

  if (query == "SomeRandomCityInTheMiddleOfNowhere") {
    return res.status(404).send({ suggestions: [] });
  }

  return res.status(200).json({
    suggestions: [
      {
        name: "Montreal, CA, Canada",
        latitude: "42.98339",
        longitude: "-81.23304",
        score: 0.9
      }
    ]
  });
});

module.exports = app;
