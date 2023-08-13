import 'dotenv/config';
import env from './config/env';
import { setupApp } from './app';

const url = env.server.url;
const port = env.server.port;

setupApp().then((app) => {
  app.listen(port, url, () =>
    console.log(`Server running at http://${url}:${port}/suggestions`)
  );
});
