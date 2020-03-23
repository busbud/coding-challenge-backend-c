module.exports = {
  server: {
    host: "0.0.0.0",
    port: process.env.PORT || 8080
  },
  db: {
    host: process.env.DB_HOST || "0.0.0.0",
    port: process.env.DB_PORT || "5432",
    user: process.env.DB_USER || "postgres",
    password: process.env.DB_PASSWORD || "postgres",
    database: process.env.DB_DATABASE || "postgres"
  },
  app: {
    cache: {
      suggestions: {
        ttl: 300
      }
    },
    weight: {
      similarity: process.env.APP_WEIGHT_SIMILARITY || 0.8
    },
    brute: {
      freeRetries: 1000,
      attachResetToRequest: false,
      refreshTimeoutOnRequest: false,
      minWait: 60 * 1000,
      maxWait: 15 * 60 * 1000,
      lifetime: 24 * 60 * 60
    }
  }
};
