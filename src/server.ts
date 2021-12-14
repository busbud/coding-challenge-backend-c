import express, {Application} from 'express'
import * as path from 'path'
import cityController from "./controller/city-controller";
import getRouter from './routes'

export default class Server {
  init(): Application {
    const app = express()
        .use(express.json())
        .use('/', getRouter()) // Local API

    return app
  }
}
