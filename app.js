import bodyParser from 'body-parser';
import express from 'express';
import logger from './config/logger.js';

import suggestionRoutes from './routes/suggestion.routes.js';

const app = express();

app.use(bodyParser.json());

app.use('/suggestions', suggestionRoutes);

const port = process.env.PORT || 2345;

export default app.listen(port, () => {
    logger.info(`Server running at http://localhost:${port}`);
});
