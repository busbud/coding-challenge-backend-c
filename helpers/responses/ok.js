
// 200 (OK) Handler
//
// Usage:
// return sendOK(res, data);

module.exports = function sendOK (res, data) {
  res.writeHead(200, {'Content-Type': 'application/json'});
  var json = JSON.stringify(data);
  return res.end(json);
};
