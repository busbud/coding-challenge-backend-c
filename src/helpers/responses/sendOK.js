
// 200 (OK) Handler
//
// Usage:
// return sendOK(res, data);

module.exports = function sendOK(res, datas) {
  res.writeHead(200, {'Content-Type': 'application/json'});
  var json = '';
  if(datas) json = JSON.stringify(datas);
  return res.end(json);
};
