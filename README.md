# Busbud Coding Challenge

## Solution explanation

This API has been designed in NestJs / Postgres and Redis.

Below the explanations of my choice.

### NestJs

This framework has been chosen in order of having objects classes, as I'm a Java developer, I make more sense for me to
continue with objects programmatic.

### Postgres

A database Postgres has been deployed with docker-compose, all data from the CSV file have been parsed into a DTO object
and stored into the DB.

An index has been created on name column to earn time on our fetches.

Two extensions have been installed, one called fuzzystrmatch and another PostGis, both are useful to generate score
object.

Fuzzystrmatch is used to provide help on distance string score. PostGIS is used to make better performance for
geographic objects.

### Flyway

To be able to easily migrate and manage our database, flyway has been added to docker-compose and a initialization
script is under resources/flyway folder.

### Redis

A redis has been deployed with docker-compose, it's important to note that the cache is only based on the keyword
provided, we won't cache based on geographic position, it's not relevant enough.

### HealthCheck

A simple healthCheck has been implemented to be able to know quickly if our API is up, we used Terminus on this.
https//xxxx/health

## Logs

Logs have been put in place for SuggestionController and SuggestionService.

## Score

With Fuzzystrmatch, we use the Levenshtein approach to calculate our score based on name. With PostGIS, we use ST_Point
and ST_SetSRID method to calculate our score based on geographic points.

## URL

The urls for the API depending on environments :

Local  : localhost:3000/suggestions?q=Montreal Heroku : localhost:3000/suggestions?q=Montreal

## How to run the application

You will need to copy the .env.example file and rename it to .env.

You can put the following entries to this new file : 
DB_URL=postgres://docker:docker@localhost:5432/suggestions
REDISPORT=6379
REDISHOST=localhost

You will need docker and docker-compose to run the application.

To run the application locally :

- npm i -g @nestjs/cli
- nvm use
- npm install
- docker-compose up
- npm run build
- npm run start

To run the tests locally :

- npm install -g jest
- nvm use
- npm install
- docker-compose up
- npm run test

