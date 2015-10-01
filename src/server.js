const restify = require('restify');
const suggest = require('./suggest');

const server = restify.createServer({
  name: 'Busbud CitySearch™'
});
server.use(restify.queryParser());

server.get('/', (req, res) => {
  res.json({
    endpoints: [{
      endpoint: '/suggestions',
      query_params: ['q', 'latitude', 'longitude']
    }]
  });
});

server.get('/suggestions', (req, res) => {

  const start = new Date().getTime();

  const queryString = req.params.q;
  const coords = {
    lat: req.params.latitude || null,
    lng: req.params.longitude || null
  };

  if (!queryString) {
    res.status(400);
    res.json({
      message: 'Querystring required'
    });
  }

  const results = suggest(server.cities, queryString, coords);

  const end = new Date().getTime();

  if (results.length === 0) {
    res.status(404);
  }
  res.json({
    total_query_time: end - start + ' milliseconds',
    suggestions: results
  });

});

module.exports = server;