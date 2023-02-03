import express, {Express} from 'express';
import {CitiesSuggestionService} from '../services/suggestion_service';
import {getCitiesDataFromFile} from '../utils/parser/tsv_parser';
import {getSuggestionRequestValidator} from './requestValidators';
import {setupSuggestionRoute} from './routes';
import {IApiConfig} from "../interfaces/interfaces";
// eslint-disable-next-line @typescript-eslint/no-var-requires
const compression = require('compression');

/**
 *  Main app to run this api server
 *  @constructor
 *  @param {number} port - HTTP port to run this app on
 *  @param {IApiConfig} config - API configuration for endpoints
* */
export class App {
  private suggestionService: CitiesSuggestionService | undefined;
  private server: Express;
  private readonly serverPort: number;
  private apiConfig: IApiConfig;

  constructor(port: number, config: IApiConfig) {
    this.server = express();
    this.server.use(compression())
    this.serverPort = port;
    this.apiConfig = config;
  }

  /** Initializes the application. */
  public async init() {
    try {
      await this.initializeServices();
      this.setupRoutes();
      this.server.listen(this.serverPort, () => {
        console.log('Server running at http://127.0.0.1:%d', this.serverPort);
      });
    } catch (e) {
      console.error('Failed to start application', e);
    }
  }

  /** Initializes services by setting up all the params needed. */
  private initializeServices() {
    return new Promise((resolve, reject) => {
      getCitiesDataFromFile(this.apiConfig.countriesSupported)
        .then((data) => {
          this.suggestionService = new CitiesSuggestionService(data);
          resolve(true);
        })
        .catch((error) => {
          reject(error);
        });
    });
  }

  /** Setup all express server routes */
  private setupRoutes() {
    if (this.suggestionService) {
      const suggestionRouteHandler = setupSuggestionRoute(this.suggestionService);
      this.server.get('/suggestions', getSuggestionRequestValidator, suggestionRouteHandler);
      console.log('setup /suggestion route');
    }
  }
}
