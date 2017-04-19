const stream = require('stream');
const getDb = require('./reader').getDb;


/**
 * Generic object used for writing to any collection of the database.
 *
 * Implements the Writable stream.
 */
export default class DatabaseWriter extends stream.Writable {

  constructor(options, fieldNamesMap, Clazz) {
    super(options);
    // this.count = 0;
    this.fieldNamesMap = fieldNamesMap;
    this.Clazz = Clazz;
  }

  _write(record, encoding, callback) {
    // View Write Progress / Data
    // this.count += 1;
    // console.log(`Record Number ${this.count}`);
    // console.log(record);

    // Create Database record data from gathered data using the right field names
    const dataForDbRecord = {};
    for (const key of Object.keys(record)) {
      dataForDbRecord[this.fieldNamesMap[key]] = record[key];
    }

    // Create and save an instance object to NeDB using 'camo' library
    getDb().then(() => {
      const instanceToSave = this.Clazz.create(dataForDbRecord);
      instanceToSave.save().then(() => {
        callback();
      });
    });
  }
}
