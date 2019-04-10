# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Requirements

Design an API endpoint that provides auto-complete suggestions for large cities.
The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

- the endpoint is exposed at `/suggestions`
- the partial (or complete) search term is passed as a querystring parameter `q`
- the caller's location can optionally be supplied via querystring parameters `latitude` and `longitude` to help improve relative scores
- the endpoint returns a JSON response with an array of scored suggested matches
    - the suggestions are sorted by descending score
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    - each suggestion has a name which can be used to disambiguate between similarly named locations
    - each suggestion has a latitude and longitude
- all functional tests should pass (additional tests may be implemented as necessary).
- the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).
- feel free to add more features if you like!

#### Sample responses

These responses are meant to provide guidance. The exact values can vary based on the data source and scoring algorithm

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

- All code should be written in Javascript
- Mitigations to handle high levels of traffic should be implemented
- Challenge is submitted as pull request against this repo ([fork it](https://help.github.com/articles/fork-a-repo/) and [create a pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)).
- Documentation and maintainability is a plus

### References

- Geonames provides city lists Canada and the USA http://download.geonames.org/export/dump/readme.txt
- http://www.nodejs.org/
- http://ejohn.org/blog/node-js-stream-playground/


## Getting Started

Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and
[Windows](http://windows.github.com/) that make this easier.

### Setting up a Erlang environment

This project is configured using Docker. Building the project and running tests
and deploys should be done through Docker. In case you need to configure and
setup your editor, the current Erlang version is `20.0.2`.

Get started by installing [Docker (Community
Edition)](https://docs.docker.com/install/).

### Fetching & building dependencies

To fetch and build dependencies run

```
$ make deps
```

### Running the app

To start a local server run

```
$ make run
```

Then visit [](http://localhost:9000/)

### Tests

In order to run the tests make sure you create a `config/test.config` file with
the right configuration.

```
$ cp config/test.config{.sample,}

# Edit config/test.config...
```

To run the tests locally:

```
$ make tests
```

## Database setup

### Starting the database

Then to start the DB locally and run all migrations, run:

```
$ make db-start
```

After that, to import the cities data, run:

```
$ make db-import-cities
```

### Updating the database

To incorporate new schema changes into the database we need to use
migrations. First, set up the database access credentials in
`config/test.config`. Then, in order to create a new migration run:

```
MIGRATION_NAME=<name> make de-create-migration
```

Migrations will be automatically run whenever you start up the testing
database. But for the production database, you should configure
`config/sys.config` first and then do:

```
$ make db-migrate-up
```

## Deploying

### Building the docker image

In order to deploy the application we first need to build a Docker image.
To do so, run:

```
$ TARGET_IMAGE=<image> make build
```

`<image>` should be the docker image name, which will be appended with
the appropriate version tag.

## Running the docker image (locally)

Set up sys.config with valid credentials (use testing credentials for
testing purposes). Then run:

```
$ IMAGE_MAME=<image>:<version> make run-prod
```

You should now be able to access the production app at [](http://localhost:9000/).
