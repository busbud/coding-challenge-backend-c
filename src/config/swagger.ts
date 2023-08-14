import swaggerConfig from '../docs';
import { noCache } from './no-cache';

import { serve, setup } from 'swagger-ui-express';
import { Express } from 'express';

export function setupSwagger(app: Express) {
  app.use('/docs', noCache, serve, setup(swaggerConfig));
}
