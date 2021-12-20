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

There are no ttl set, as a city doesn't change its name every day :)

### HealthCheck

A simple healthCheck has been implemented to be able to know quickly if our API is up, we used Terminus on this.
https://tranquil-reaches-74242.herokuapp.com/health
http://localhost:3000/health

## Logs

Logs have been put in place for SuggestionController and SuggestionService.

## Swagger

A swagger is available at this url : http://localhost:3000/api
Even if we only have one endpoint, it's always useful to have a documentation clear and simple for others to interact
with our API.

## Throttling

To avoid IP's attacks, there is throttling on this API, it has been put on app.module.ts with the following config :
ttl: 60, limit: 20

Which means, you can do 20 requests per minute with the same IP.

## Score

With Fuzzystrmatch, we use the Levenshtein approach to calculate our score based on name. With PostGIS, we use ST_Point
and ST_SetSRID method to calculate our score based on geographic points.

## API Security

To secure our API, an X-API-KEY is required as a header. You will need to add in your Postman an Authorization, choose
type : api key Params = key:X-API-KEY, value:testBusBudForPerrineApi

### Heroku

The integration to Heroku, I added Postgres and Redis add-ons and run the two flyway scripts located in the resources
folder. And I added config variables needed in Heroku.

## URL

The urls for the API depending on environments :

Local  : localhost:3000/suggestions?q=Montreal
Heroku : https://tranquil-reaches-74242.herokuapp.com/suggestions?q=Londo&latitude=48.70011&longitude=-73.4163

## How to run the application

You will need to copy the .env.example file and rename it to .env.

You can put the following entries to this new file :
DB_URL=postgres://docker:docker@localhost:5432/suggestions REDISPORT=6379 REDISHOST=localhost REDISPASSWORD=
API_KEY=testBusBudForPerrineApi ENTITY_PATH=dist/**/**/*.entity{.js,.ts}

You will need docker and docker-compose to run the application. To run the application locally :

- npm install
- docker-compose up
- npm run build
- npm run start

To run the tests locally :

DB_URL=postgres://docker:docker@localhost:5432/suggestions ENTITY_PATH=dist/**/**/*.entity{.js,.ts} PORT=3000
REDISPORT=6379 REDISPASSWORD= REDISHOST=localhost

- npm install -g jest
- npm install
- docker-compose up
- npm run test

## One more thing

I know that I have high vulnerabilities with axios, the report is telling
'axios  <=0.21.1', I forced the axios' version to 0.21.4, the axios folder contains this 0.21.4 version but it's still
telling me that my version deprecated. I'm sorry about this issue, if you have any idea to fix this issue, let me know. 
