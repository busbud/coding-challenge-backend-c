const suggestions = require('./suggestions');
let controllers = null;

module.exports = function (client, db, config) {
  if (controllers) {
    return controllers;
  }

  controllers = {};
  controllers.suggestions = suggestions(client, db, config);


  return controllers;
};