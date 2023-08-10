import 'dotenv/config';
import { setupApp } from './app';

const url = process.env.SERVER_URL || '127.0.0.1';
const port = parseInt(process.env.SERVER_PORT ?? '') || 2345;

const app = setupApp();

app.listen(port, url, () =>
  console.log(`Server running at http://${url}:${port}/suggestions`)
);
