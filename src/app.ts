import config from 'config';
import cors from 'cors';
import express from 'express';
import helmet from 'helmet';
import routes from './routes';

const app = express();

app.set('trust proxy', true);
app.use(express.json());
app.use(cors());
app.use(helmet());

app.listen(config.port, () => console.log(`Application started on 127.0.0.1:${config.port}`));

app.use('/', routes());

app.get('*', (req, res) => res.json({ alive: true }));

export default app;
