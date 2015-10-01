require("babel/register");
import express from "express";
import React from "react";
import Router from "react-router";
import createLocation from "history/lib/createLocation";
import { RoutingContext, match } from "react-router";
import tooBusy from "toobusy-js";
import router from "./router";
import mongodb, { MongoClient } from "mongodb";
import pmongo from "promised-mongo";

const port = process.env.PORT || 2345;
const app = express();

// Configure toobusy-js
tooBusy.maxLag(400); // maximum lag permitted. Default: 70ms
tooBusy.interval(500); // lag scan interval. Default: 500ms

// In a real environment we would store the db credentials somewhere that isn't committed into code
//const db = pmongo("mongodb://localhost/busbud-challenge?maxPoolSize=10");
const db = pmongo("mongodb://busbud:theflowmustspice@ds033113.mongolab.com:33113/heroku_cgt4f4mj?maxPoolSize=10");

// Serve static assets from the public directory
app.use(express.static('public'));

// Load our router, passing the db
app.use(router(db));

app.listen(port, function () {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

export default app;
