import express, { Response, Request } from 'express';
import 'express-async-errors';
import { json } from 'body-parser';
import { NotFoundError } from './errors/not-found-error';
import { errorHandler } from './middlewares/error-handler';
import { suggestions } from './routes/suggestions-route';
// import { TryDBConnect } from './data/connection';

const app = express();
app.set('trust proxy', true);
app.use(json());
// app.use(async (req: Request, res: Response, next) => {
//   await TryDBConnect(() => {
//     res.json({
//       error: 'Database connection error, please try again later',
//     });
//   }, next);
// });
app.use(suggestions);

app.all('*', async (_: Request, res: Response) => {
  throw new NotFoundError();
});

app.use(errorHandler);

export { app };
