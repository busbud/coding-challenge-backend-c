import express from 'express';
import morgan from 'morgan';

const app = express();

app.use(morgan(process.env.MORGAN_FORMAT || 'dev'));

app.get('/suggestions', (_req, res) => {
  res.json({ data: 'Hello World' });
});

export default app;
