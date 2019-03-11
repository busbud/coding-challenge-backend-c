module.exports = {
  development: {
    db: 'mongodb://localhost/code_challenge',
    redis: 'redis://localhost/',
    port: process.env.PORT || 2345
  },
  production: {
    db: process.env.MONGO_URI,
    redis: process.env.REDIS_URI,
    port: process.env.PORT
  }
};