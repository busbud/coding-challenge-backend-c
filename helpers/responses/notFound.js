
// 404 (Not Found) Handler
//
// Usage:
// return notFound(res);
 
module.exports = function notFound (res, datas) {
  res.writeHead(404, {'Content-Type': 'application/json'});
  var json = '';
  if(datas) json = JSON.stringify(datas);
  return res.end(json);
};
