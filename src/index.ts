import 'dotenv/config';
import express, { NextFunction, Request, Response } from 'express';

const app = express();

(async () => {
  // Middleware
  app.use(express.json());

  // Routes
  app.use('/suggestion', () => {});

  // Basic error handling
  app.use((err: any, req: Request, res: Response, next: NextFunction) => {
    console.error(err.stack || err.message);
    if (err.statusCode) res.status(err.statusCode).json(err);
    else res.status(500).json({ message: 'Uh, something went wrong' });
  });

  app.listen(process.env.EXPRESS_PORT, () => {
    console.log(`Live at ${process.env.EXPRESS_PORT}!`);
  });
})();
