const config = require('../../../../config');

const internalError = (res, reason) => {
  console.log(reason);

  const body = {
    error: 'Something went wrong',
  };
  if (config.env !== 'production') {
    body.detail = reason;
  }

  res.writeHead(500, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify(body));
};

const badRequest = (res, reason) => {
  console.log(reason);
  const body = { error: reason };

  res.writeHead(400, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify(body));
};

module.exports = {
  internalError,
  badRequest,
};
