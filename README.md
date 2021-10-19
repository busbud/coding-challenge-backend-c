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

## Summary of the challenge

The application is deployed in the following Heroku dyno: https://busbudcodechallenge.herokuapp.com/suggestions

The stack selected was NodeJS + Typescript using PostgreSQL database to store the countries/provinces/cities tables.
Typeorm was selected to manage database entities due to its maturity and ease to implement
Redis was selected as a cache engine since it is fast and scalable solution

## Dev environment setup

### Steps to start the dev environment

* Make sure that NodeJS (LTS version) and npm are installed

* Clone the repository at https://github.com/RenanFragoso/coding-challenge-backend-c
```
git clone https://github.com/RenanFragoso/coding-challenge-backend-c.git
```

* go to the cloned folder and run the npm install command
```
npm install
```

* create the .env file with the environment variables mentioned on the following topic

* open a terminal, go to the project root's folder, and run the following command to start the source compilation in watch mode
```
npm run dev:tsc:watch
```
* open a second terminal, go to the project root's folder, and run the following command to initiate the database structure (assuming that postgre client is installed in the same environment)

```
createdb -h <host> -p <port> -U <user> testdb
```

* Check if the database connection string is correct in .env file

* using the same terminal, run the following command to initiate the database structure
```
npm run typeorm:runmigration
```

* using the same terminal, run the following command to start the server in watch mode for source changes
```
npm run dev:server:watch
```

When running the server for the first time, it will check for data present in the postgre database and start populating from the data files if no cities are found

If typeorm gets too verbose, it is possible to disable the logging feature by setting logging: false in src/config/getTypeormConfig.ts

### Environment variables (.env file)

.env files will not be present in the codebase (and should never be due to security concerns)

The creation of .env file is necessary when running the application in the local environment. 
The following values are mandatory in order for the application to run properly 

PORT=2345
HOST=0.0.0.0
DATABASE_URL=postgres://<user>:<password>@<host>:<port>/<database>
REDIS_URL=redis://<host>:<port>
DISTANCE_KM_FACTOR=100
DISTANCE_PENALTY_FACTOR=10
DATAFILE_COUNTRIES=src/data/country_iso_names.tsv
DATAFILE_PROVINCES=src/data/provinces_admin1_codes.tsv
DATAFILE_CITIES=src/data/cities_canada-usa.tsv

* PORT: server port to listen
* HOST: internet interface to listen for connections
* REDIS_URL: redis uri connection string used by the cache
* DATABASE_URL: postgre URI connection string used by typeorm
* DISTANCE_KM_FACTOR: this is the factor used by calculateDistance function in order to decrease the word score by 1/DISTANCE_PENALTY_FACTOR each DISTANCE_KM_FACTOR kilometers
* DISTANCE_PENALTY_FACTOR: is the factor penalty applied to word score per each DISTANCE_KM_FACTOR kms distant from the query informed point
* DATAFILE_COUNTRIES: path containing the countries tsv file 
* DATAFILE_PROVINCES: path containing the provinces tsv file 
* DATAFILE_CITIES: path containing the cities tsv file 

### Environment data 

The folder src/migrations will already contain a generated migration file. 
The migration file is a result of typeorm client for the command: migration:generate.
Migration files contain all database DDL commands in order to maintain the database structure synced with source code entities, it will contain the update (up) and rollback (down) commands.

After changing any entity (src/models/entities) structure, an update in the corresponding database table is necessary. The command to generate the migration is available through npm:

```
$ npm run typeorm:generatemigration "migrationName"
```

After generating the migration file, the following command is necessary in order to apply the changes. This command will apply all pending migrations (generated migrations not synced with the database)

```
$ npm run typeorm:runmigration
```

Case a rollback is necessary, the following command will revert the last migration

```
$ npm run typeorm:revertmigration
```

Use those commands with caution since they can destroy the data

### Performance considerations

Based on the following query command, I decided that a search string with less than 3 characters is not necessary since it will not leave any city out of the result set (the minimum city name length found in the data file is 3). This will be used as one of the mitigation solutions to avoid unnecessary requests to the database or cache

```
SELECT CHAR_LENGTH("name"), "name"
FROM cities
ORDER BY CHAR_LENGTH("name") ASC
```

As a second mitigation solution, a cache using Redis was implemented.
The Redis cache will store suggestions based on the query string after the first time a query is received. Score values are always recalculated since the latitude and longitude can change per each query request and those values do not affect the dataset returned as result of the query string search in the database

### Unit tests

Few unit tests were added to the solution (*.spec.ts), mostly to the score calculation functions

TODO: Due to the complexity added to the project, structure change, and time restrictions, tests related to the controller could not be properly implemented. Mocking database and cache connections to a dedicated test environment will require a little more effort to complete.