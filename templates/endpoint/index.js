/**
 * this is not used to make the copy

 to change anything here for the newly created endpoint look at cli/endpoint.js
*/

const auth = require("@app/auth");
const { endpoint } = require("./units");
module.exports = async ({ req }) => {
  return await auth({ req }, async (user) => {
    return await endpoint();
  });
};
