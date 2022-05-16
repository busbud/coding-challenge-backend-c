import dotenv from 'dotenv';
import herokuConfig from 'config/heroku';

dotenv.config();

export default {
  port: herokuConfig.dyno
    ? herokuConfig.appPort
    : Number(process.env.APP_PORT),
};
