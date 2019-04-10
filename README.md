# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

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

Then visit [localhost:9000](http://localhost:9000/).

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

## Testing

In order to run the tests make sure you create a `config/test.config` file with
the right configuration.

```
$ cp config/test.config{.sample,}

# Edit config/test.config...
```

To run the tests locally (with the local db started and cities
imported):

```
$ make tests
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
$ IMAGE_NAME=<image>:<version> make run-prod
```

You should now be able to access the production app at [localhost:9000](http://localhost:9000/).
