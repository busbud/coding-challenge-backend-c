const { Transform } = require("stream");

/**
 * Stream or discard chunk based on bool returned from callback
 *
 * @param {function} fn
 * @returns { Transform }
 */
const streamFilter = fn => {
  return new Transform({
    objectMode: true,
    transform(row, encoding, cb) {
      const keep = fn(row);
      cb(null, keep ? row : undefined);
    }
  });
};

/**
 * Process the chunk through the callback function and stream the result to the destination stream.
 *
 * @param {function} fn
 * @returns { Transform }
 */
const streamMap = fn => {
  return new Transform({
    objectMode: true,
    transform(row, encoding, cb) {
      const result = fn(row);
      cb(null, result);
    }
  });
};

/**
 * Make it more generic and name it something like promisfyTransform
 *
 * @param {function} fn
 */
const streamToMongoBulkWrite = fn => {
  let documents = [];
  const promises = [];

  return new Transform({
    objectMode: true,
    transform(row, encoding, cb) {
      documents.push({ insertOne: { document: row } });
      if (documents.length === 1000) {
        promises.push(fn(documents));
        documents = [];
      }
      cb();
    },
    flush(cb) {
      if (documents.length > 0) {
        promises.push(fn(documents));
      }
      Promise.all(promises)
        .then(() => {
          cb(null, "DONE");
        })
        .catch(err => {
          cb(err);
        });
    }
  });
};

exports.streamFilter = streamFilter;
exports.streamMap = streamMap;
exports.streamToMongoBulkWrite = streamToMongoBulkWrite;
