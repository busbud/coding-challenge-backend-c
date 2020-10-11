const BodyParser = require("body-parser");
const compression = require("compression");
const expressSanitizer = require("express-sanitizer");
const SuggestionRoutes = require("../routes/SuggestionRoutes.js");

/**
 * @method
 * @description has `load` method to configure express app
 * @param {Express} app
 * @return {Object}
 */
module.exports = (app) => {
  return {
    /**
     * @method load
     * @description configures express app with defined rules
     */
    async load() {
      app.use(
        BodyParser.json({
          type: function () {
            return true;
          },
        })
      );
      app.use(expressSanitizer());
      app.use(compression());
      app.use("/", SuggestionRoutes);
    },
  };
};
