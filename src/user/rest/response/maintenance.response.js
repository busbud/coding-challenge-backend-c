const response = (res, status, httpStatus) => {
  res.writeHead(httpStatus, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({
    status,
  }));
};

module.exports.response = response;
