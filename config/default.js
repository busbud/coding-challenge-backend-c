module.exports = {
  server: {
    ip: "0.0.0.0",
    port: process.env.PORT || 8080,
    host: process.env.HOST || "localhost",
  },
  db: {
    url:
      process.env.DATABASE_URL ||
      "mongodb+srv://admin:12345678!A@suggestion.qq64t.mongodb.net/suggestion?retryWrites=true&w=majority",
    collection: "cities",
    options: {
      useNewUrlParser: true,
      useUnifiedTopology: true,
      useCreateIndex: false,
    },
  },
};
