import 'dotenv/config';
import env from './config/env';
import { setupApp } from './app';

const url = env.serverUrl;
const port = env.serverPort;

const app = setupApp();

app.listen(port, url, () =>
  console.log(`Server running at http://${url}:${port}/suggestions`)
);
