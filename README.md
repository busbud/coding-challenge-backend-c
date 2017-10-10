# Cities Suggestion Engine

![Travis Badge](https://travis-ci.org/barodeur/cities-suggestion-engine.svg?branch=master)

_This project is an implementation of the [Busbud coding challenge](https://github.com/busbud/coding-challenge-backend-c)._

## API Documentation

- The API is public, there is no authentication required to access the endpoint.
- There is nly a single endpoint available 
- The API only replies in `application/json`

### Request with matching results

**REQUEST**

```
GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
```

**RESPONSE**

```
200
```

```json
{
  "suggestions": [
    {
      "name": "London, ON, Canada",
      "latitude": "42.98339",
      "longitude": "-81.23304",
      "score": 0.9
    },
    {
      "name": "London, OH, USA",
      "latitude": "39.88645",
      "longitude": "-83.44825",
      "score": 0.5
    },
    {
      "name": "London, KY, USA",
      "latitude": "37.12898",
      "longitude": "-84.08326",
      "score": 0.5
    },
    {
      "name": "Londontowne, MD, USA",
      "latitude": "38.93345",
      "longitude": "-76.54941",
      "score": 0.3
    }
  ]
}
```

### Request without matching results

**REQUEST**

```
GET /suggestions?q=SomeRandomCityInTheMiddleOfNowhere
```

**RESPONSE**

```
404
```

```json
{
  "suggestions": []
}
```


## Implementation details

- This suggestion engine relies heavily on elasticsearch
- The results are filtered using a prefix query on the name of the cities
- then the score is calculated using a gaussian of the distance to the search location


## Contribute

### Requirements
- node 8
- npm 5
- postgresql
- elasticsearch

### Start
- Run `npm install`
- Import a sample of the cities into the postgresql database by running `./tasks/import-data.js`
- Create or Recreate the elasticsearch index and the index the cities with `./tasks/index-cities.js`
- You can now start the server: `node app.js`
