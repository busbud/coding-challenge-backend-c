import express from 'express';
import morgan from 'morgan';

import router from './router';

const app = express();

app.use(morgan(process.env.MORGAN_FORMAT || 'dev'));

app.use('/', router);

export default app;
