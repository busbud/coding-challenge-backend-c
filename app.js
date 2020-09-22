import express from 'express';
import rateLimit from 'express-rate-limit';
import bodyParser from 'body-parser';
import logger from './config/logger.js';

import erroHandler from './error/error.genericHandler.js';
import suggestionRoutes from './routes/suggestion.routes.js';

const app = express();

app.use(bodyParser.json());

app.set('trust proxy', 1);
const limiter = rateLimit({
  windowMs: 60 * 1000,
  max: 1000,
  message: {},
});
app.use(limiter);

app.use('/suggestions', suggestionRoutes);

app.use(erroHandler.genericErrorHandler);

const port = process.env.PORT || 2345;

export default app.listen(port, () => {
  logger.info(`Server running at http://localhost:${port}`);
});
