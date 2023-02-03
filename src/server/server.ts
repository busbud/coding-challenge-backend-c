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

const port = Number(process.env.PORT) || 1234;



export async function createServer() {
    const server = express();
    server.use(compression())

    if (suggestionService) {
        const suggestionRouteHandler = setupSuggestionRoute(suggestionService);
        server.get('/suggestions', getSuggestionRequestValidator, suggestionRouteHandler);
    } else {
        await getCitiesDataFromFile(apiConfig.countriesSupported)
            .then((data) => {
                suggestionService = new CitiesSuggestionService(data);
                const suggestionRouteHandler = setupSuggestionRoute(suggestionService);
                server.get('/suggestions', getSuggestionRequestValidator, suggestionRouteHandler);
            })
            .catch((error) => {
                console.log("Failed to get cities data from file. cannot start api server", error)
                process.exit(1)
            });
    }
    return server;
}


export async function startServer(){
    const app = await createServer();
    await app.listen({ port: port }, () => {
        console.log(`Server running at http://127.0.0.1:%d/suggestions at process id %s`, port, process.pid)
    });
}