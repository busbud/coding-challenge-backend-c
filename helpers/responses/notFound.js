
// 404 (Not Found) Handler
//
// Usage:
// return notFound(res);
 
module.exports = function notFound (res) {
  res.writeHead(404, {'Content-Type': 'application/json'});
  return res.end();
};
