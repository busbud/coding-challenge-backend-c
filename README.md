# Busbud Coding Challenge

## Submission Documentation

Thank you for giving me the opportunity to participate in this code challenge! 

A deployed version of my solution on Heroku can be found [here](https://busbud-cc-suggestions.herokuapp.com/suggestions).

The instructions for building and configuring my solution locally are listed below. I have also provided some notes on requirements and any assumptions made.

## Notes & Assumptions

- In order to optimize performance of the lookups, I created and stored indexes for each city name in a Redis store. The initial load of data can be performed manually by running `npm run load`, or it can be performed with every launch of the application by setting the environment variable `LOAD_DATA` to `true`. The load script takes approximately a minute to complete, but I felt this was a reasonable tradeoff in return for instant suggestion lookups during runtime. Future versions of the application could perform incremental updates of the indexes, but this version wipes and re-creates the indexes with each load.
- The records for Canadian cities in the provided TSV file had a numerical code for the territory (province) (e.g. 01), where the American cities had the standard two letter abbreviation (e.g. WA). The final result output makes use of the code, so fully qualified Canadian city names have the numerical code in place of the province abbreviation. Future versions could use this code in a lookup for the appropriate province code.
- The requirements for the challenge indicated that only North American cities with a population of over 5000 should be loaded. I added checks in to ensure this, but it appears all of the provided data met these constraints anyway. I also made the population threshold configurable.
- API traffic control is achieved through the Node package `express-rate-limit`. The set limit is 200 requests per 15 minute window. Future versions of the application would make this configurable through environment variables. 
- The algorithm used to calculate result scores is the following:
  - The prefix (partial name) provided is compared to the full (ASCII) name for each result and the difference in length is divided by the length of the full city name and subtracted from 1.0, giving a final value between 0.0 (no match) to 1.0 (full match).
  - If latitude/longitude coordinates are provided in a query, then each result score is adjusted based on whether the distance between it's coordinates and the specified query coordinates falls below or above a configured threshold (defaults to 50). If the distance is greater than the threshold, the configured penalty is applied to that score (defaults to 0.2). If the distance is less than the threshold, the configured bonus is applied to that score (defaults to 0.1).
  - Results are ordered in descending order by their final, adjusted scores.
- If the query parameter (q) is not provided in the request, then a status code 400 error is returned.
- If no results are found, then a status code 404 is returned with an empty array.
- The latitude and longitude query parameters are optional. If none, or only one of the two is provided, no adjustment of the scores is performed.

## Build Requirements

- Node.js v14.x (can be installed via `nvm`)
- Docker or a local Redis instance (for local development only).

## Building and Running Locally 

1. Clone this repository and navigate to the project root.
2. Run `docker-compose up -d` to launch a Redis instance. If desired, first edit the `docker-compose.yaml` file to change the default authentication parameters for Redis.
3. Run `npm install` to install package dependencies.
4. Create a `.env` file with the required environment variables defined. A `sample.env` has been provided. See below for more info.
5. Run `npm run load` to parse the TSV data file and load the city data into Redis.
6. Run `npm start` to launch the application. 

### Environment variables

| Name | Description |
| --- | --- |
| `HOST` | The IPv4 address for the back-end to bind to. Default to `3000` |
| `PORT` | The TCP port for the back-end to bind to. Defaults to `0.0.0.0`. This variable needs to be set to `0.0.0.0` for deployment on Heroku. |
| `REDIS_URL` | The Redis key-value store to connect to. If this is not set, then the other `REDIS_x` environment variables must be provided. |
| `REDIS_HOST` | The host for the Redis key-value store. Used to generate a `REDIS_URL` environment variable. |
| `REDIS_PORT` | The port for the Redis key-value store. Used to generate a `REDIS_URL` environment variable. |
| `REDIS_USER` | The user for authenticating with the Redis key-value store. Used to generate a `REDIS_URL` environment variable. |
| `REDIS_PWD` | The password for authenticating with the Redis key-value store. Used to generate a `REDIS_URL` environment variable. |
| `DATA_FILE_PATH` | The file path to the TSV data file containing lookup data. |
| `DATA_ID_INDEX` | The column index in the TSV data file that contains record IDs. |
| `DATA_NAME_INDEX` | The column index in the TSV data file that contains name data. |
| `DATA_ASCII_NAME_INDEX` | The column index in the TSV data file that contains name data in ASCII format. |
| `DATA_LAT_INDEX` | The column index in the TSV data file that contains latitude coordinates. |
| `DATA_LONG_INDEX` | The column index in the TSV data file that contains longitude coordinates. |
| `DATA_COUNTRY_INDEX` | The column index in the TSV data file that contains country code data. |
| `DATA_TERR_INDEX` | The column index in the TSV data file that contains territory (state/province) code data. |
| `DATA_POPULATION_INDEX` | The column index in the TSV data file that contains city population data. |
| `DATA_CITY_MIN_POP` | The minimum population threshold that defines which cities to include in the lookup. |
| `LOC_RADIUS_THRESHOLD` | The distance radius boundary from which to distinguish score bonuses and penalties when lookup coordinates are provided. Defaults to 50 km. |
| `DIST_SCORE_BONUS` | The score bonus to apply when a result is within the `LOC_RADIUS_THRESHOLD` of the specified coordinates. Defaults to 0.1. |
| `DIST_SCORE_PENALTY` | The score penalty to apply when a result is within the `LOC_RADIUS_THRESHOLD` of the specified coordinates. Defaults to 0.2. |
| `LOAD_DATA` | Whether to perform a data load on each launch of the application. Defaults to false. |

## Testing

To run the integration tests in Mocha, execute the script `npm test`.

---

## Original README 

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
- feel free to add more features if you like!

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

## Dataset

You can find the necessary dataset along with its description and documentation in the [`data`](data/) directory.

## Evaluation

We will use the following criteria to evaluate your solution:

- Capacity to follow instructions
- Developer Experience (how easy it is to run your solution locally, how clear your documentation is, etc)
- Solution correctness
- Performance
- Tests (quality and coverage)
- Code style and cleanliness
- Attention to detail
- Ability to make sensible assumptions

It is ok to ask us questions!

We know that the time for this project is limited and it is hard to create a "perfect" solution, so we will consider that along with your experience when evaluating the submission.

## Getting Started

### Prerequisites

You are going to need:

- `Git`
- `nvm` (or your preferred node version manager)
- `Node.js`

### Setting up your environment

1. Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and
[Windows](http://windows.github.com/) that make this easier.

2. Install [nvm](https://github.com/nvm-sh/nvm#install--update-script) or your preferred node version manager.

3. Install [Node.js](http://www.nodejs.org).

### Setting up the project

In the project directory run:

```
nvm use
npm install
```

### Running the tests

The test suite can be run with:

```
npm run test
```

### Starting the application

To start a local server run:

```
npm run start
```

it should produce an output similar to:

```
Server running at http://127.0.0.1:2345/suggestions
```
