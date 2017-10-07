# Cities Suggestion Engine

> ⚠️ This project is a work in progress, there is no stable version at the moment.

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
200
```

```json
{
  "suggestions": []
}
```


## Implementation details

TODO


## Contribute

- Make sure you're running node 8 and you use npm 5
- Run `npm install`