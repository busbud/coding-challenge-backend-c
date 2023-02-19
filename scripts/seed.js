/**
 * In a real world scenario this script would be a process running every N days. It would query the data source (postgres, mysql, etc.), 
 *  Build tries for each letter of the alphabet, and then save the tries to redis cache. This code isn't meant to be part of the actual microservice
 *  so my focus here isn't to make the cleanest code
 */


const castToLocationObject = require('../models/Location');
const { createInterface } = require('readline');
const { createReadStream } = require('fs');
const Trie = require('../lib/trie');

// Source File
const DATASOURCE = './data/cities_canada-usa.tsv';

/**
 * Stream data from data source. This would normally be done by whatever ORM you are using. Mongoose, Sequelize, etc.
 * @param {Function} runOnRow function to run on each row of the data source
 * @param {Function} onFinish function to run once the data source has been fully streamed
 */
function streamByRow(runOnRow, onFinish) {
  const readStream = createReadStream(DATASOURCE);
  var lineReader = createInterface({
    input: readStream
  });

  lineReader.on('line', function (line) {
    runOnRow(line);
  });

  lineReader.on('error', (err) => {
    readStream.close();
  });

  readStream.on('close', () => {
  });

  lineReader.on('close', () => {
    readStream.close();
    onFinish();
  })
}

/**
 * Builds Tries for each letter of the alphabet
 * @param {Function} onComplete callback once the tries are built
 */
function buildTries(onComplete) {
  const tries = {};
  streamByRow((row) => {
    const location = castToLocationObject(row);
    const firstLetter = location.name[0].toLowerCase();
    if (!tries[firstLetter]) {
      tries[firstLetter] = new Trie();
    }
    tries[firstLetter].insertWord(location);
  }, () => onComplete(tries));
}


/**
 * Batch job that would run and buil the redis cache used by the microservice
 * @param {Function} redisCache function to save to redis cache
 */
function seed(onFinish) {
  buildTries(onFinish);
}

module.exports = seed;
