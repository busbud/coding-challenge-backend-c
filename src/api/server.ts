import express from 'express';
import { connect, Connection } from 'api/db';
import { getConfigErrors, PORT } from 'api/config';
import { makeRouter } from 'api/router';

export class Server {
  private app;

  constructor() {
    this.app = express();
    this.config();
  }

  public async start(): Promise<number> {
    // Check for configuration errors, and abort if there are any.
    const errors = getConfigErrors();
    if (errors.length > 0) {
      errors.forEach((error) => console.log(error));
      throw new Error('Invalid environment variable configuration.');
    }

    // Connect to database
    const connection = await connect()
    .catch(e => {
      console.error(e);
      throw new Error('Unable to connect to the data store.');
    });

    // Configure the route(s)
    await this.routes(connection);

    // Listen and resolve with port.
    return new Promise((resolve, reject) => {
      this.app.listen(PORT, () => {
        resolve(PORT);
      })
      .on('error', (err) => reject(err));
    });
  }

  config() {
    this.app.use(express.urlencoded({ extended: true }));
    this.app.use(express.json({ limit: '1mb' }));
  }

  async routes(connection: Connection) {
    this.app.use('/suggestions', await makeRouter(connection));
  }
}
