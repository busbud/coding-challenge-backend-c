import express from 'express';
import routes from './routes';
import rateLimit from 'express-rate-limit';
import CityLoader from './services/loaders/CityLoader'

const loader = new CityLoader();
class App {
    private _exp: express.Application;

    constructor() {
        this._exp = express();
        this.middleware();
        this.router()
    }

    private middleware() {
        const limiter = rateLimit({
            windowMs: 10000,
            max: 20,
            message: 'Too many requests from this IP, please try again after 10 seconds'
        });
        this._exp.use(limiter);
    }


    private router() {
        this._exp.use(express.json());
        this._exp.use(routes);
    }

    initialize(): express.Application {
        console.log('Initializing indexes')
        loader.loadCitiesFromTsv('data/cities_canada-usa.tsv')
            .then(() => console.log('Initialized indexes'))
            .catch((error: any) => {
                console.error(error);
                process.exit(1);
            });
        const port = process.env.PORT || 3333;
        this._exp.listen(port, () => {
            console.log('Server running at http://127.0.0.1:%d/suggestions', port);
        });
        return this._exp;
    }

}

export default new App().initialize();