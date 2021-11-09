require('dotenv').config()

import express from 'express';

const PORT = process.env.APP_PORT || 8000;

const app = express();
app.get('/', (req, res) => res.send('Busbud Server'));
app.listen(PORT, () => {
  console.log(`⚡️[server]: Server is running at https://localhost:${PORT}`);
});