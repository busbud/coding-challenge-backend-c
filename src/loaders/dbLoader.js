const mongoose = require("mongoose");
const { migrateToDB, tsvToJSON } = require("../../lib/seeds.js");
const config = require("config");

const uri = config.get("db.url");
const dbOptions = config.get("db.options");

/**
 * @description loader object contains db related actions
 */
module.exports = {
  /**
   * @method load
   * @description connect db and check data || migrate data
   */
  async load() {
    await mongoose
      .connect(uri, dbOptions)
      .then(async () => {
        console.info("Mongo connection successful");
        /**
         * Check if collection doesn't exist
         * Migrate .tsv data to mongodb
         */
        await this.checkCollectionExists().catch(
          async (error) => await this.dataParse()
        );
      })
      .catch((err) => {
        console.error("Mongo connection error", err);
      });
  },

  /**
   * @method checkCollectionExists
   * @description check if the collection is ready for api
   */
  checkCollectionExists() {
    return new Promise((resolve, reject) => {
      return mongoose.connection.db
        .listCollections()
        .toArray((error, names) => {
          if (error || names.length == 0) {
            console.info("Mongo collection will be created");
            reject(error);
          } else resolve(names);
        });
    });
  },

  /**
   * @method dataParse
   * @description parse/migrate data from tsv to db
   */
  async dataParse() {
    const data = await tsvToJSON({
      inputPath: "./data/cities_canada-usa.tsv",
    });
    await migrateToDB({ data });
    console.info("Mongo collection has been created");
  },
};
