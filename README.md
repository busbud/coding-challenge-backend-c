# Busbud Coding Challenge

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

- All code should be written in Javascript or Typescript.
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

## Candidate Notes

### Description

Implementation of a term search with optional location parameterization.
The result of the search is a suggestion of cities ordered by name similarity and proximity.

Example: 

- [/suggestions?q=montreal&latitude=40.4314779&longitude=-80.0507118](https://anishitani-busbud.herokuapp.com/suggestions?q=montreal&latitude=40.4314779&longitude=-80.0507118)
  
``` json
{
  "suggestions": [
    {
      "name": "Montreal, 10, CA",
      "latitude": "45.50884",
      "longitude": "-73.58781",
      "score": 0.6857142857142857
    },
    {
      "name": "Montreal-Ouest, 10, CA",
      "latitude": "45.45286",
      "longitude": "-73.64918",
      "score": 0.4820182953011256
    }
  ]
}
```

#### Parameters

- `q` => The lookup term (required)
- `latitude`, `longitude` (optional)

#### Features

- Searches are matched fully or partially, meaning that results for `Mont` will be a  subset of results of `Montreal` lookup.
- Cached queries by term, latitude and longitude parameters.
- Configurable rate limit on the resource.
- Requests queue after reaching rate limit.

### Running the application

It's possible to run the app using `docker` and `docker-commpose`.

```sh
docker-compose up
```

### Parameters

Some properties can be set to for a finer tune of the application.

#### Server

- `HOST`: The address the application will accept requests. (default `0.0.0.0`)
- `PORT`: The port the application will accept requests. (default `8080`)

#### Database

The application makes use of a Postgres Database. 
It must be set beforehand and it may be configured through the following properties.

- `DB_HOST`: DB server hostname (default `0.0.0.0`)
- `DB_PORT`: DB server listening port (default `5432`)
- `DB_USER`: User to connect with the database. (default `postgres`)
- `DB_PASSWORD`: Password to connect with the Database. (default `postgres`)
- `DB_DATABASE`: The name of the database. (default `postgres`)

#### Application  

##### Cache

- `APP_CACHE_SUGGESTION_TTL`: The `time to live` of each key in seconds. (default 300)

##### Weight

- `APP_WEIGHT_SIMILARITY`: How much relevance is given to term similarity. Value is given between 0 and 1. The left over is the location relevance. (default 0.8)

##### Rate Limiter

- `APP_RATE_LIMITER_POINTS`: Number of requests alowed per `duration`. (default 100)
- `APP_RATE_LIMITER_DURATION`: Window for counting points in seconds. (default 1)
- `APP_RATE_LIMITER_BLOCK_DURATION`: Blocking period in seconds after reaching points limit. (default 5)

##### Rate Limiter Queue

- `APP_RATE_LIMITER_QUEUE_MAX_QUEUE_SIZE`: Queue length to store requests after reaching points threshold. (default 100)