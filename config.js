module.exports = {
  env: process.env.NODE_ENV,
  server: {
    host: 'localhost',
    port: (process.env.PORT || 2345)
  }
};