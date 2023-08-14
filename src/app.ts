import express from 'express';
import { setupRateLimit } from './config/rate-limit';
import setupRoutes from './config/routes';
import { setupSwagger } from './config/swagger';
import { setupRedisClient } from './config/redis';

export async function setupApp() {
  const app = express();

  setupSwagger(app);
  setupRateLimit(app);
  await setupRedisClient(app);
  setupRoutes(app);

  app.disconnect = () => {
    app.redis.quit();
  };

  return app;
}
