# AUTO COMPLETE API DOCUMENTATION

This aim of the API is to provide a list of suggestions that contains the cities that match the user request.

## How to use the API ?

The API exposes an endpoint /suggestions 

It takes three query string parameters:
 - q: the partial (or complete) search term | mandatory
 - latitude: caller location latitude | optional
 - longitude: caller location longitude | optional

## How to run the API locally ?

In order to run the API in your local machine, you need to:
 - install posgtresql server
 - clone the project
 - CREATE a ".env" file from ".env-template" file and update the values
 - run the command : npm run initdb
 - run the command : npm run start


## How it is working internally ?

- This API is based on posgtresql database. 
- In order to check the similarity between the user request and the city name in the db, I am using the similarity function of the pg_trgm extension.
- In order to check the distance between the longitude, lattitude passed by the caller and the ones of the city in the database, I am using <@> operator of the cube and earthdistance extensions.

## What could be improved ?

A caching service could implemented (e.g. redis) to avoid making several database requests if the same information was already computed.