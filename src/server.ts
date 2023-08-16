import express from 'express';
import morgan from 'morgan';

import { rateLimiter } from './modules/middleware';
import router from './router';

const app = express();

app.use(rateLimiter);
app.use(morgan(process.env.MORGAN_FORMAT || 'dev'));

app.use('/', router);

export default app;
