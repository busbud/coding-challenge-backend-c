const dbLoader = require("./dbLoader.js");
const expressLoader = require("./expressLoader.js");

/**
 * @description prepares the app with express and db
 * @param {Express} app
 */
module.exports = async (app) => {
  await expressLoader(app).load();
  await dbLoader.load();
};
