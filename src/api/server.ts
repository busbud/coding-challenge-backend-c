import express, { Express } from 'express';
import { connect, Connection } from 'api/db';
import * as config from 'api/config';
import { makeRouter } from 'api/router';
import rateLimit from 'express-rate-limit';

export class Server {
  private app: Express;

  constructor(app: Express) {
    this.app = app; 
    this.config();
  }

  public async init(): Promise<Express> {
    // Check for configuration errors, and abort if there are any.
    const errors = config.getConfigErrors();
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
    this.routes(connection);

    // Set up rate limiting to control traffic for all requests
    const limiter = rateLimit({
      windowMs: 15 * 60 * 1000,
      max: 200,
      message: 'Too many requests, please try again later.'
    });

    this.app.use(limiter);

    this.app.listen(config.PORT, config.HOST)
    console.info(`Server running at http://${config.HOST}:${config.PORT}/suggestions`)

    if (config.LOAD_DATA) 
      require('../load');

    return this.app;
  }

  config() {
    this.app.use(express.urlencoded({ extended: true }));
    this.app.use(express.json({ limit: '1mb' }));
  }

  routes(connection: Connection) {
    this.app.use(makeRouter(connection));
  }
}
