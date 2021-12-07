## Requirements
Design an API endpoint that provides autocomplete suggestions for large cities.
The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

- the endpoint is exposed at `/suggestions`
- the partial (or complete) search term is passed as a query string parameter `q`
- the caller's location can optionally be supplied via query string parameters `latitude` and `longitude` to help improve relative scores
- the endpoint returns a JSON response with an array of scored suggested matches
    - the suggestions are sorted by descending score
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    - each suggestion has a name which can be used to disambiguate between similarly named locations
    - each suggestion has a latitude and longitude
- all functional tests should pass (additional tests may be implemented as necessary).
- the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).

#### Sample responses

These responses are meant to provide guidance. The exact values can vary based on the data source and scoring algorithm.

**Near match**

    GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

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

**No match**

    GET /suggestions?q=SomeRandomCityInTheMiddleOfNowhere

```json
{
  "suggestions": []
}
```
### Non-functional

- All code should be written in Javascript, Typescript or PHP.
- Mitigations to handle high levels of traffic should be implemented.
- Challenge is submitted as pull request against this repo ([fork it](https://help.github.com/articles/fork-a-repo/) and [create a pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)).
- Documentation and maintainability is a plus.

# Getting started
### Prerequisites
- `nodejs >= 10` 
- `docker`

### Running containers and seeding the dataset
- `docker compose up -d`
- `npm run seed`

### Running the application
- `npm run dev` (development)
- `npm run build` && `npm run start` (deployment)

### Running tests
- `npm run test`

### Running from Heroku
```
curl --request POST 'https://busbud-cc.herokuapp.com/suggestion?q=london on'
```
```
curl --request POST 'https://busbud-cc.herokuapp.com/suggestion?lat=35.65283&long=-97.4781&q=dall'
```

### Notes
- Secrets are stored in Heroku.
- Caching with Redis improved response time to ~1/3.

### What would be next
A few things to consider moving from here...
- Logging, metrics and error handling still need to properly be addressed.
- Fine tune eslint and probably tsconfig for production.
- Fine tune redis and elastic search.
- Pre-commit/push using Husky.
- Integration and e2e tests.

### Bonus
In case you're using VSCode, here's a launch config that might help if you need to debug the application.
```json
{  
      "name":"TSND",
      "type":"node",
      "request":"launch",
      "protocol":"inspector",
      "cwd":"${workspaceRoot}",
      "runtimeExecutable":"${workspaceRoot}/node_modules/.bin/ts-node-dev",
      "args":[  
         "${workspaceRoot}/src/index.ts"
      ],
      "restart":true
}
```