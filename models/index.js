'use strict';

const glob = require(`glob-all`);
const log = require('../lib/logger')('models-index');

const modelFiles = glob.sync([`./*`, `!./index.js`, `!./*.spec.js`], {cwd: __dirname}).map(file => file.split('.js')[0].split('./')[1]);

for(const modelFile of modelFiles) {
  try {
    exports[modelFile] = require(`./${modelFile}`);
  } catch(e) {
    log.f(`Error loading model "${modelFile}"`, e);
  }
}

log.i(`${modelFiles.length} model file(s) loaded`);
