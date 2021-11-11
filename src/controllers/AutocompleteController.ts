import SearchService from "../services/SearchService";
import { SuggestionsRequest } from "../types";

//** Suggestions endpoint
const suggestions: SuggestionsRequest = async (request, response, next) => {
  const params = request.query;

  const location = params.latitude && params.longitude
    ? { lat: params.latitude, lon: params.longitude}
    : undefined;

  const {
    hits: { hits, max_score },
  } = await SearchService.search({
    term: params.q,
    location,
    limit: params.limit,
    offset: params.offset,
  });

  /*
  @TODO: Move this logic to ES Query, selecting
  only the needed fields to preserve network resources 
  */
  const suggestions = hits.map(({ _source, _score }) => {
    return {
      name: `${_source.name}, ${_source.admin1_code}, ${_source.country_code}`,
      latitude: _source.location.lat,
      longitude: _source.location.lon,
      score: _score * (1 / max_score),
    };
  });

  const status = suggestions.length > 0 ? 200 : 404;
  response.status(status).send({ suggestions });

  next();
};

export default {
  suggestions,
};
