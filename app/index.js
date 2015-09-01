import express from 'express';
import suggestions from './routes/suggestions';

const app = express();

app.use(suggestions);
app.use(express.static('public'));

export default app;
