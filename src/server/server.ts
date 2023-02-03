import express from 'express';
import {CitiesSuggestionService} from '../services/suggestion_service';
import {getCitiesDataFromFile} from '../utils/parser/tsv_parser';
import {getSuggestionRequestValidator} from './requestValidators';
import {setupSuggestionRoute} from './routes';
import {IApiConfig} from "../interfaces/interfaces";
// eslint-disable-next-line @typescript-eslint/no-var-requires
const compression = require('compression');

const countriesSupported = process.env.COUNTRIES_SUPPORTED?.split(',') || ['CA', 'US'];
const populationLimit = Number(process.env.POPULATION_LIMIT) || 5000;
const apiConfig: IApiConfig = { countriesSupported, populationLimit }

let suggestionService: CitiesSuggestionService;

export const server = express();
server.use(compression())

getCitiesDataFromFile(apiConfig.countriesSupported)
    .then((data) => {
      suggestionService = new CitiesSuggestionService(data);
      const suggestionRouteHandler = setupSuggestionRoute(suggestionService);
      server.get('/suggestions', getSuggestionRequestValidator, suggestionRouteHandler);
    })
    .catch((error) => {
      console.log("Failed to get cities data from file. cannot start api server", error)
      process.exit(1)
    });
