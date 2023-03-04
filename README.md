# Busbud Coding Challenge - Suggestions API

This document serves as the descriptive information source of the Suggestions API

## Deployed application

The deployed application can be accessed at
* https://busbud-coding-challenge-eqzlm6b3nq-nn.a.run.app/

#### Other important README files
- [RUNBOOK](docs/runbook.md)
- [ARCHITECTURE](docs/architecture.md)
- [Updating Firestore Database](docs/refreshFirestoreDatabase.md)

## Suggestions API
### Endpoint
`{base-url}/suggestions`

### Description
Returns a list of suggested cities based on the keyword provided as a query parameter. Optionally, longitude and latitude can also be provided to increase the quality of results. Please note for refining results on the basis of latitude and longitude, both of them should be provided otherwise the results are completely based on the keyword.

#### HTTP Verb
`GET`

### Produces
`application/json`

### Parameters
|   Name    |               Description                |  in   | Required |  Type  |
|:---------:|:----------------------------------------:|:-----:|:--------:|:------:|
|     q     | Search text targeting name of the cities | query |   true   | string |
| longitude |          Longitude of the user           | query |  false   | number |
| latitude  |           Latitude of the user           | query |  false   | number |

## Request Schema
`Path:{base-url}/suggestions?q=${q}&longitude={country}&latitude=${latitude}`

### Response By Status

###### When queries are successful, the HTTP response is a `200 OK` and returns a list of suggested cities

`Response Example`
```json
{
   "suggestions":[
      {
            "name": "Ammon",
            "latitude": 43.46964,
            "longitude": -111.96664,
            "score": 0.5422670977568499,
            "stringSimilarity": 0.6,
            "distanceScore": 0.40755699252283295
        },
        {
            "name": "Ione",
            "latitude": 38.35269,
            "longitude": -120.93272,
            "score": 0.497342941876628,
            "stringSimilarity": 0.5,
            "distanceScore": 0.49114313958876005
        },
        {
            "name": "Pomona",
            "latitude": 34.05529,
            "longitude": -117.75228,
            "score": 0.4866921886745146,
            "stringSimilarity": 0.5,
            "distanceScore": 0.45564062891504875
        }
   ]
}
```

#### Errors

| Response code |                            Reason                             |
|:-------------:|:-------------------------------------------------------------:|
|      400      | Request contains validation errors or missing required params |
|      500      |                 Error within the Application                  |
