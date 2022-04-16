import express from 'express';
import bodyParser from 'body-parser';
import cors from 'cors';
import type { Application } from 'express';
import config from '../../app/config';

export default class HttpServer {
  private server: Application;
 
  public init(): void {
    this.create();
    this.configurate();
  }

  public run() {
    this.server.listen(Number(config.api.port), config.api.hostname, () => {
      console.info(`
                ################################################
                      Server listening on: ${config.api.hostname}:${config.api.port}
                ################################################
            `);
    });
  }

  private create(): void {
    this.server = express();
  }

  private configurate(): void {
    this.server.use(bodyParser.json());
    this.server.use(cors());
  }
}
