import app from 'app';
import appConfig from 'config/app';
import nodeConfig from 'config/node';
import dockerConfig from 'config/docker';
import herokuConfig from 'config/heroku';

app.listen(appConfig.port);

process.stdout.write(
  `Server listening on port ${appConfig.port} in ${nodeConfig.env} environment\n`,
);

if (dockerConfig.appContainer) {
  process.stdout.write(
    `Server running in Docker app container, available at http://127.0.0.1:${dockerConfig.appPort}\n`,
  );
}

if (herokuConfig.dyno) {
  process.stdout.write(
    'Server running in Heroku dyno\n',
  );
}
