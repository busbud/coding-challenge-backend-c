"use strict";

const { runUp, COLLECTION } = require("./../src/suggestions/import");
const { connect, mongo, getClient } = require("./../src/mongo");
const { waitForApp } = require("./../src/dbUtils");

module.exports.up = async function() {
  connect();
  await waitForApp(5000, 5, true);
  const db = mongo();
  await runUp(db);
  await db.collection("locations").createIndex({ testGeo: "2dsphere" });
  getClient().close();
};

module.exports.down = async function() {
  connect();
  await waitForApp(5000, 5, true);
  await mongo()
    .collection(COLLECTION)
    .remove();
  getClient().close();
};
