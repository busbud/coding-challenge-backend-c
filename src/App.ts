import express from 'express';
import routes from './routes';
import CityLoader from './services/loaders/CityLoader'
import { trafficLimit } from './middleware/trafficLimit';
import cors from 'cors';

class App {
    private _loader: CityLoader = new CityLoader();
    private _exp: express.Application;

    constructor() {
        this._exp = express();
        this.middleware();
        this.router()
    }

    private middleware() {
        this._exp.use(cors());
        this._exp.use(trafficLimit);
        this._exp.use(express.json());
    }

    private router() {
        this._exp.use(routes);
    }

    initialize(): express.Application {
        console.log("Initializing indexes");
        this._loader.loadCitiesFromTsv('data/cities_canada-usa.tsv')
            .then(() => {
                const port = process.env.PORT || 3333;
                this._exp.listen(port);
                console.log('Server running at http://127.0.0.1:%d/suggestions', port);
            })
            .catch(() => process.exit(1));
        return this._exp;
    }

}

export default new App().initialize();