import express from 'express';
import routes from './routes';
import CityLoader from './services/loaders/CityLoader'
import { trafficLimit } from './middleware/trafficLimit';
import cors from 'cors';

class App {
    private _cityLoader: CityLoader = new CityLoader();
    private _application: express.Application;

    constructor() {
        this._application = express();
        this.middleware();
        this.router()
    }

    private middleware() {
        this._application.use(cors());
        this._application.use(trafficLimit);
        this._application.use(express.json());
    }

    private router() {
        this._application.use(routes);
    }

    initialize(): express.Application {
        console.log("Initializing indexes");
        this._cityLoader.loadCitiesFromTsv('data/cities_canada-usa.tsv')
            .then(() => {
                const port = process.env.PORT || 3333;
                this._application.listen(port);
                console.log('Server running at http://127.0.0.1:%d/suggestions', port);
            })
            .catch(() => process.exit(1));
        return this._application;
    }

}

export default new App().initialize();