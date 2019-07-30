# Busbud Coding Challenge [![Build Status](https://travis-ci.org/bendamqui/coding-challenge-backend-c.svg?branch=master)](https://travis-ci.org/bendamqui/coding-challenge-backend-c)



## Introduction

This service was implemented using Node.js, MongoDB and Redis. It get deployed to https://morning-bastion-69807.herokuapp.com/ through Travis CI. It runs under version 10.16.0 of Node. For developement, a docker-compose file is available so it can run on any machine.

## Local Environment Infrastructure

In order to work properly the app rely on three services. 

* `api` which is build from node:10.16.0-alpine and run on port 80.
* `mongo` using bitnami/mongodb:4.1 image.
* `redis` from redis:5.0-alpine

In addition to the required docker services the docker-compose file spin
up a mongo-express container which provide a UI to visualize the data stored in MongoDB. It is accessible on port 8081.

Docker doesn't play any role in the deployment process. Even if we don't solve the "it works on my machine but not on the server" problem, it remains a tool that makes developers life much easier.

On commit, the code is reformat by prettier.

## Production Infrastructure

The production environment run of course on the same stack as the local environment except that
heroku addons are used.

## Node Dependencies

The node version have been update from 0.10.26 to 10.16.0. In the process node modules have also been updated in response to several vulnerability warnings and errors.

There are no modules that deserve special attention among the added dependencies but it might be intersting to note that mongoose and a framework such as expressjs haven't been installed. 

As far as mongoose is concern, after some experimentations, it looks like its strengh rely mainly in its Models which seem to be bypassed quite easily when the need to perform more complex queries or use more exotic methods arise. For this reason and because we don't need any complex data modeling the node-mongodb package have been picked and is used to communicate with the database.

As for expressjs framework, it might have made a file or two look cleaner but nothing to write home about. That been said, booting an expressjs server take about the same time than finding a reason not to use it. So, maybe it was a mistake.


## Tests

The test infrastructure is the same that was initially set in the repo. New test files have been added and some refactoring have been made on the existing code in order to make the addition of new tests easier.

The tests in `suggestions.js` can been seen as integration test whereas the other files tend more 
toward unit testing.

## CI/CD

Travis CI is responsible of continuous integration. Every push to the repo trigger the build and run the tests. A push to master will also deploy to heroku.

## Data Strategy

The data are imported via db-migrate into mongo. They are massaged on import so they can be consumed with no additional processing. 

Every column get saved so they could potentially be used by other endpoints and they could allow  modifications to the way the score is calculated without running a migration.

A geo index is created on the location collection to allow the score to be entirely calculated from
the query. The motivation to calculate the score on mongo comes from the need to perform a sort and to apply a limit. Not applying a limit on the query would potentially affect the server for no good reasons while applying a limit before calcuting the score ( if we suppose that the score is calculated in javascript) would mean that a document with a perfect score could potentially be ignored.

Redis takes care of caching the results in order to improve performance. The keys are built using the search term and rounded geo inputs. Gathring metrics on the application usage could give informations
that could guide us on how to create more optimal keys.

## Search Strategy

The location name input from the user is transform in such a way that it becomes a spaceless, accentless, regex-characterless string that we used to perform a match on a field in the DB that received similar treatment during the import. 

The idea is to forgive some error from the user on input. We are still far from fuzzy search but it looks like MongoDD is not the most appropriate tool to perform such operations.

## Score Strategy

The name of the location, the size of the city and the distance weight in the calculation of the score. 

The score for the name is calculated based on how close from the begining of the location name we find a match. Thus, the search
`mont` will score better against `Montreal` than `ontr`

Obviously location with greater population get an higher score and the ones that are closer from where
the user is also get an higher score.


## Desing Pattern / Stream

The usage of functional programing desing pattern have been widely used for "pedagogical" purpose. Same goes for the streams. You can find some abstract functional programing function in `fp.js` as well as you'll find some stream implementations in `transform.js`. 

## Instalation
 
 Make sure Docker is installed on your machine.

Make a copy of `.env.example` and rename it to `.env`

Run the following comands to build and run the containers.

```docker-compose build```

```docker-compose up```

Run the migration

```docker-compose exec api npm run migrate -- up```

Run the tests

```docker-compose exec api npm run test```

You can access the api at http://localhost/suggestions

## Conclusion 

I think that the choice of MongoDB over something like a lucene based search engine might have been overly optimistic. Some benchmarking
would be required to see if a more appropriate tool would really have an impact on performance on such a small set of data but it's for sure 
worth the try.

The functional programming experience was quite a good exercice for the brain. On my first readings on the subject I was quite sceptical
on claim such as "it make the code easy to test/read" but got pleasantly surprised when the time came to write tests and refactor some part of 
the code.











