/** a simple helper to return a json response with a specific code */
/* istanbul ignore next */
function withCode(response, code, body = {}) {
  const stringBody = JSON.stringify(body);
  if (code === 200) return response.end(stringBody);
  return response.status(response).end(stringBody);
}

module.exports = {
  withCode
};
