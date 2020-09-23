# Busbud Coding Challenge
# Solution overview

## Infrastructure

### PostgreSQL

The data available was imported into a PostgreSQL database with the PostGIS extension installed. PostGIS is a spatial extension that adds extra types and functions to our database, making it easy to perform operations using Geographic Information objects. The database installation also contains the fuzzystrmatch module that provides some functions to determine distance between string. Both modules were used to generate the result score

### Migration

Flyway was used to manage our database migrations and make it easy to recreate the database in any enviroment

### How the score is calculated

A really simple and naive approach was used to calculate the similarity between the result and the `searchTerm` provided by the user. The metric uses was the Levenshtein distance - it measures the distance between two string using the number of character edits required to transform one word into the other. For instance, the Levenshtien distance between `London` and `Londo` is 1. This approach is easy to but has some drawbacks. For instance, the distance between `mouse` and `house` is also 1, and they are cleary different words. After finding the result score it applied a normalization to set the min and max values between 0 and 1.

When the latitude and longitude is provided is used the PostGIS nearest-neighbour search (in addition to Levenshtien). Applying the function, ordering the result and limiting the result set we can find the N nearest cities from the provided lat/lon. After finding the distance it is also applied normalization to set the min and max values between 0 and 1

The final score is the weighted average based 0.3 * Levenshtien_score + 0.7 * KNN distance

### Other details

- Controllers methods are wraped inside a wrapAsync function. This helps us avoid any unhandled exception inside our code - Any unhandled exception should be catched by the genericErrorHandler automatically. 

- DTO validations is performed before the controllers call to avoid validation logic inside the controllers and/or services.

- A simple rate-limiting middleware is also being used to avoid too many requests coming from the same IP. This is useful when the API provided is public. but to avoid issues caused by service unavailabilty or errors the client applications should implement a `circuit break` strategy. 

### Some missing features

- Caching layer
- Monitoring, health endpoints, etc 
- Could have more tests :(

### Application URL



|Env  |URL                                                                         |
|-----|----------------------------------------------------------------------------|
|Local  |localhost:2345/suggestions?q=Montreal |
|Heroku |https://tranquil-lowlands-29057.herokuapp.com/suggestions?q=Montreal  |


### Starting the application

A docker-compose file is provided in order the run the application locally (for the database and migrations)

* It is required to have docker and docker-compose installed.
  - Install docker on ubuntu https://docs.docker.com/engine/install/ubuntu/ (or your other favorite distro :)
  - Install docker-compose on ubuntu https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-compose-on-ubuntu-20-04


To start a local server run:

```
// Set node version and install dependencies
nvm use
npm install
```
```
// Start postgreSQL container and run migrations
docker-compose up
```
```
// Run the app :) 
npm run hot-reload
or
npm start 
```

### Testing the app

```
nvm use
npm install

docker-compose up
npm test
```

To clean up the enviroment run docker-compose rm