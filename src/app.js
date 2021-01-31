const express = require("express");
const cors = require("cors");
const routes = require("./routes");
const { createError, getServerPort } = require("./utils");
const { createLocationsTable } = require("./db/createLocationsTable");

const app = express();

app.set("port", getServerPort());

app.use(cors());
app.use(express.json());
app.use(express.urlencoded({ extended: true }));

routes(app);

// 404 handler
app.use(function (req, res, next) {
  const error = createError(404, "Not found!");
  next(error);
});

// error handler
app.use(function (err, req, res, next) {
  res.status(err.status || 500);
  res.json({
    error: err.message,
  });
});

app.listen(app.get("port"), async () => {
  console.log(`Busbud app listening at http://localhost:${app.get("port")}`);
  try {
    await createLocationsTable();
    console.log("Db created successfully!");
  } catch (err) {
    console.error("Something goes wrong on creation db!", err);
  }
});
