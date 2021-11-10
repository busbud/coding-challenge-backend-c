import elasticsearch from 'elasticsearch';

import RequestContract from "../types/RequestContract";
import SearchService from "../services/SearchService";

//** Suggestions endpoint
const suggestions: RequestContract = async (request, response, next) => {
  const { q: term, latitude, longitude, limit = 50, offset = 0 } = request.query;
  
  const { hits: { hits, max_score } } = await SearchService.search({
    term,
    location: latitude && longitude ?{
      lat: latitude,
      lon: longitude,
    } : undefined,
    limit, 
    offset,
  })
  
  const suggestions = hits.map(({ _source, _score }: any) => {
    return {
      "name": `${_source.name}, ${_source.admin1_code}, ${_source.country_code}`, 
      "latitude": _source.location.lat,
      "longitude": _source.location.lon,
      "score": _score * (1 / max_score),
    };
  })
  
  const status = suggestions.length > 0 ? 200 : 404;
  response.status(status).send({ suggestions })

  next();
}

export default ({ 
  suggestions 
});