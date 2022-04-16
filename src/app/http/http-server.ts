import express from 'express';
import bodyParser from 'body-parser';
import cors from 'cors';
import type { Application } from 'express';
import config from '../config';

export default class HttpServer {
  public server: Application;
 
  public start(): void {
    this.create();
    this.setup();
    this.run();
  }

  private create(): void {
    this.server = express();
  }

  private setup(): void {
    this.server.use(bodyParser.json({ limit: '64mb' }));
    this.server.use(cors());
  }

  private run() {
    this.server.listen(Number(config.api.port), config.api.hostname, () => {
      console.info(`
                ################################################
                      Server listening on: ${config.api.hostname}:${config.api.port}
                ################################################
            `);
    });
  }
}
