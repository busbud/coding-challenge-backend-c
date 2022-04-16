import { Application } from 'express';
import CitySuggestions from './city/suggestions';

export default (app: Application) => {
    CitySuggestions(app);
}
