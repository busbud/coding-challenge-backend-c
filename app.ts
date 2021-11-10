require('dotenv').config()
import express from 'express';
import router from './src/routes';

const PORT = process.env.APP_PORT || 8000;
const app = express();

app.use('/', router);

app.listen(PORT, () => {
  console.log(`⚡️[server]: Server is running at http://localhost:${PORT}`);
});


export default app;