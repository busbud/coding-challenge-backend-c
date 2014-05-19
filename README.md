# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Demo
http://morning-ridge-2285.herokuapp.com/suggestions?q=Lon

## Performance
* Run through a reverse proxy and cache responses at the HTTP level
* Improve cacheability by using at most 2 decimal digits for longitude and latitude (https://en.wikipedia.org/wiki/Decimal_degrees)
* Run the app from `cluster.js` to make use of all available cores

## API

### `/suggestions`

#### Parameters
* `q`: Term in need of auto-completion (**Required**)
* `longitude`: User's longitude (Optional)
* `latitude`: User's latitude (Optional)

#### Response
The response contains an array of cities sorted by relative score, as follows:

    GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

```json
{
  "suggestions": [
    {
      "id": 12345,
      "name": "London, ON, Canada",
      "latitude": "42.98339",
      "longitude": "-81.23304",
      "score": 0.9
    },
    {
      "id": 23456,
      "name": "London, OH, USA",
      "latitude": "39.88645",
      "longitude": "-83.44825",
      "score": 0.5
    }
  ]
}
```

**NOTE**: The scoring function assumes that the user is searching for a bus trip in a city around their current location.
The score is calculated as the inverse of the squared euclidian distance between the user and the result city.

**NOTE #2**: In addition to the fields in the requirements, this endpoint also returns a numerical `id` field to have a more direct match with the TSV database.

## Configuration
* `process.env.PORT`: API listening port; default is `2345`
* `process.env.RESOURCE`: API endpoint, default is `/suggestions`
* `process.env.DATAFILE`: GeoNames data file, default is `./data/cities_canada-usa.tsv`

## Technical details
* The HTTP endpoint is implemented with Express JS, facilitating query parsing, endpoint definition and JSON serialization
* Added CORS headers
* The auto-completion engine is Bloodhound, part of [Twitter's typeahead.js project](https://github.com/twitter/typeahead.js)
* Bloodhound was modified to make it compatible with Node. The pull-request is at https://github.com/twitter/typeahead.js/pull/853
* [Node's cluster module](http://nodejs.org/api/cluster.html) can be used to run the app on more than one core. **NOTE**: Memory requirements for the index are multiplied by the number of cores.
