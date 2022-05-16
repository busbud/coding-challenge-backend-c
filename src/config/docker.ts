import dotenv from 'dotenv';

dotenv.config();

export default {
  appContainer: Boolean(Number(process.env.DOCKER_APP_CONTAINER)),
  appPort: Number(process.env.DOCKER_APP_PORT),
};
