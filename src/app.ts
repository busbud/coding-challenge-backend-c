import express from 'express';
import { setupRateLimit } from './config/rate-limit';
import setupRoutes from './config/routes';
import { setupRedisClient } from './config/redis';

export async function setupApp() {
  const app = express();

  setupRateLimit(app);
  await setupRedisClient(app);
  setupRoutes(app);

  app.disconnect = () => {
    app.redis.quit();
  };

  return app;
}
