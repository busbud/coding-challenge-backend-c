const fs = require("fs");
const es = require("event-stream");
const config = require("../config.json");

function readData() {
  return new Promise((resolve, reject) => {
    const result = [];

    try {
      getReadStream()
        .on("error", error => {
          reject(error);
        })
        .on("data", data => {
          result.push(data);
        })
        .on("end", () => {
          resolve(result);
        });
    } catch (ex) {
      reject(ex);
    }
  });
}

function getReadStream() {
  return fs
    .createReadStream(config.filePath)
    .on("error", error => {
      throw error;
    })
    .pipe(es.split("\n"))
    .pipe(
      es.mapSync(function(data) {
        return data.split("\t");
      })
    );
}

module.exports = readData;
