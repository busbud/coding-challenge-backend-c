# AUTO COMPLETE API DOCUMENTATION

This aim of the API is to provide a list of suggestions that contains the the cities that match the user request.

## How to use the API

The API exposes an endpoint /suggestions 

It takes three query string parameters:
 - q: the partial (or complete) search term | mandatory
 - latitude: caller location latitude | optional
 - longitude: caller location longitude | optional

## How to run the API locally

In order to run the API in your local machine, you need to:
 - install posgtresql server
 - clone the project
 - run the command : npm run initdb
 - run the command : npm run start