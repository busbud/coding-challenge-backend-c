import { Express, Router } from 'express';
import { readdirSync } from 'fs';
import { join } from 'path';
import { generateContext } from '../middleware/generate-context';

export default function (app: Express) {
  const router = Router();
  app.use(generateContext, router);
  readdirSync(join(__dirname, '../routes')).map(async (file) => {
    if (!file.endsWith('.map')) {
      (await import(`../routes/${file}`)).default(router);
    }
  });
}
