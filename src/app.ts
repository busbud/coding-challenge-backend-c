import express from 'express';
import setupRoutes from './config/routes';

export function setupApp() {
  const app = express();

  setupRoutes(app);

  return app;
}
