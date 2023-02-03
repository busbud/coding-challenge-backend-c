import { App } from './server/server';

const countriesSupported = process.env.COUNTRIES_SUPPORTED?.split(',') || ['CA', 'US'];
const populationLimit = Number(process.env.POPULATION_LIMIT) || 5000;
const port = Number(process.env.PORT) || 1234;

const app = new App(port, { countriesSupported, populationLimit });
app
  .init()
  .then(() => {
    console.log('App initialized');
  })
  .catch((e) => {
    console.error('Failed to start app', e);
  });
