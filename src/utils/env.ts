import env from 'dotenv';

if (process.env.ENV == 'LOCAL') {
  env.config();
}
