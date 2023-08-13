import express from 'express';
import { setupRateLimit } from './config/rateLimit';
import setupRoutes from './config/routes';
import { setupRedisClient } from './config/redis';

export async function setupApp() {
  const app = express();

  setupRateLimit(app);
  await setupRedisClient(app);
  setupRoutes(app);

  app.disconnect = () => {
    app.redis.disconnect();
  };

  return app;
}
