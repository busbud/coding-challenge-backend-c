const score = require("string-score");

module.exports = {
  scoreName: (text, pattern) => score(text, pattern, 0.8)
};
