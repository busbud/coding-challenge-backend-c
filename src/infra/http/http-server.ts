import express from 'express';
import bodyParser from 'body-parser';
import cors from 'cors';
import type { Application } from 'express';
import { errors } from 'celebrate';
import config from '../../app/config';
import http from 'http';

export default class HttpServer {
  private application: Application;
  private instance: http.Server;

  public init(): void {
    this.create();
    this.configurate();
  }

  public run() {
    // errors() middleware must be declared just before server launch
    this.application.use(errors());

    this.instance = this.application.listen(Number(config.api.port), config.api.hostname, () => {
      console.info(`
                ################################################
                      Server listening on: ${config.api.hostname}:${config.api.port}
                ################################################
            `);
    });
  }

  public getApplication(): Application {
    return this.application;
  }

  public getInstance(): http.Server {
    return this.instance;
  }

  private create(): void {
    this.application = express();
  }

  private configurate(): void {
    this.application.use(bodyParser.json());
    this.application.use(cors());
  }
}
