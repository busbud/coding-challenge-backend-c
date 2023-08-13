import express from 'express';
import setupRoutes from './config/routes';
import { setupRedisClient } from './config/redis';

export async function setupApp() {
  const app = express();

  await setupRedisClient(app);
  setupRoutes(app);

  app.disconnect = () => {
    app.redis.disconnect();
  };

  return app;
}
