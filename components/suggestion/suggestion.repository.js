import fs from 'fs';
import connection from '../../config/pg.js';

const GET_SUGGESTIONS_BY_TERM = fs.readFileSync('./resources/queries/get-suggestion-name.sql', { encoding: 'UTF-8' });
const GET_SUGGESTIONS_BY_TERM_AND_COORDINATES = fs.readFileSync('./resources/queries/get-suggestion-coord.sql', { encoding: 'UTF-8' });

const getSuggestionsBySearchTerm = async (searchTerm) => connection.executeQuery(GET_SUGGESTIONS_BY_TERM, [searchTerm]);

const getSuggestionBySearchTermAndCoordiantes = async (searchTerm, latitude, longitude) => connection
  .executeQuery(GET_SUGGESTIONS_BY_TERM_AND_COORDINATES, [searchTerm, longitude, latitude]);

export default {
  getSuggestionsBySearchTerm,
  getSuggestionBySearchTermAndCoordiantes,
};
