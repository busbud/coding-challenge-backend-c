import express, {Application} from 'express'
import * as path from 'path'
import cityController from "./controller/city-controller";
import getRouter from './routes'
import rateLimit from 'express-rate-limit'

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100 // limit each IP to 100 requests per windowMs
});


export default class Server {
  init(): Application {
    const app = express()
        .use(express.json())
        .use('/', getRouter()) // Local API
        .use(limiter)
        // Enable if you're behind a reverse proxy (Heroku, Bluemix, AWS ELB or API Gateway, Nginx, etc)
        // see https://expressjs.com/en/guide/behind-proxies.html
        .set('trust proxy', 1)


    return app
  }
}
