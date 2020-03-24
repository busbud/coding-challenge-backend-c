module.exports = {
  server: {
    host: process.env.HOST ||"0.0.0.0",
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
        ttl: process.env.APP_CACHE_SUGGESTION_TTL || 300
      }
    },
    weight: {
      similarity: process.env.APP_WEIGHT_SIMILARITY || 0.8
    },
    rateLimiter: {
      points: process.env.APP_RATE_LIMITER_POINTS || 100, // Limits 30 req/s. Still needs tunning.
      duration: process.env.APP_RATE_LIMITER_DURATION || 1, // Window of 1 second.
      blockDuration: process.env.APP_RATE_LIMITER_BLOCK_DURATION || 5 // Block of 5 seconds after limit reached.
    },
    rateLimiterQueue: {
      maxQueueSize: process.env.APP_RATE_LIMITER_QUEUE_MAX_QUEUE_SIZE || 100 // Queues 100 requests before throwing 429 error.
    }
  }
};
