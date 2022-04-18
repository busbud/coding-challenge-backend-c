![Busbud](https://espresso-jobs.com/files/pictures/busbud-logo-norm-rgb-hr-136.png)
# Busbud Coding Challenge

## TL;DR
- [Typescript](https://www.typescriptlang.org/) is the main language
- Install [docker](https://docs.docker.com/get-docker/) & [docker-compose](https://docs.docker.com/compose/install/)
- Install [make](https://linuxhint.com/install-make-ubuntu/) package
- Run `make install`
- Run `make start_dev`
- And that's it! You can use the new version of the Busbud places API ;)

## API

This API helps you to find your next trip location, by making it easier for you to get Cities suggestions.

### Suggestions route

    GET /suggestions?q=Montréal&latitude=45.50884&longitude=-73.58781

```json
{
  "suggestions": [
      {
          "name": "Montréal, CA",
          "latitude": 45.50884,
          "longitude": -73.58781,
          "score": 10
      },
      {
          "name": "Montréal-Ouest, CA",
          "latitude": 45.45286,
          "longitude": -73.64918,
          "score": 8
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

## Getting Started

### Prerequisites

You are going to need:

- `git`
- `docker`
- `docker-compose`
- `make`

### Setting up your environment

1. Install [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
4. Install [docker](https://docs.docker.com/get-docker/)
5. Install [docker-compose](https://docs.docker.com/compose/install/)
6. Install [make](https://linuxhint.com/install-make-ubuntu/)

### Setting up the project

#### Start docker
```
sudo service docker start
```

#### In the project directory run:

```
make install
```

### Running the tests

The test suite can be run with:

```
make test
```

### Starting the application

#### To start a local server run:

```
make start_dev
```

#### It should produce an output similar to:

```
################################################
      Server listening on: 0.0.0.0:2345
################################################
```

## Documentation
### Project structure
```
📦coding-challenge-backend-c
 ┣ 📂.husky                       => pre-commit hooks
 ┣ 📂data                         => datasets
 ┣ 📂database                     => database migrations & environments
 ┣ 📂docker                       => docker configuration files
 ┣ 📂src                          => sources directory
 ┃ ┣ 📂app
 ┃ ┃ ┣ 📂config                   => app configurations
 ┃ ┃ ┣ 📂dev
 ┃ ┃ ┃ ┗ 📜data-loader.ts         => inject dataset in database
 ┃ ┃ ┣ 📂routes
 ┃ ┃ ┃ ┣ 📂city
 ┃ ┃ ┃ ┃ ┗ 📜suggestions.ts       => suggestions action
 ┃ ┣ 📂domain
 ┃ ┃ ┣ 📂model
 ┃ ┃ ┃ ┣ 📂entity
 ┃ ┃ ┃ ┗ 📂value-object
 ┃ ┃ ┗ 📂scorer                   => suggestions scorer strategy
 ┃ ┣ 📂infra
 ┃ ┃ ┣ 📂http
 ┃ ┃ ┃ ┗ 📜http-server.ts         => express abstraction
 ┃ ┃ ┣ 📂orm
 ┃ ┃ ┃ ┣ 📜orm-client-interface.ts
 ┃ ┃ ┃ ┗ 📜sequelize-client.ts    => sequelize abstraction
 ┃ ┃ ┗ 📂tsv
 ┃ ┃ ┃ ┗ 📜reader.ts              => tsv reader
 ┗ ┗ 📂test
```
All files and folders have not been listed here for readability.

### Main design patterns 
Most of them come from the [DDD](https://en.wikipedia.org/wiki/Domain-driven_design) approach,
and add a much better decoupling compared to a classic NodeJS app.

#### ADR-like
The [ADR](https://github.com/pmjones/adr) design pattern (Action-Domain-Responder) separates each route in a distinct action class, all the business rules are in the domain directory, and the responder is a class that creates the response.
We used an ADR-like pattern, AD (Action-Domain), to lighten code. 

#### HTTP output: Presenters
Instead of preparing the View normalisation itself, an Action should delegate
all that kind of job to dedicated Presenter classes - they are just classes
that receive pure Domain data (which they decorate)
in their constructors, and "present" this data in the way the REST client
expects it.
