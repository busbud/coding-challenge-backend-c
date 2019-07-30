# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Notes:

### API url:
- https://ian-test-test.herokuapp.com/suggestions?q=londo&latitude=43.70011&longitude=-79.4163

### DB
I used elasticsearch (bonsai.io) as the db for this server which ends up being faster
and better than loading the whole data into memory.

### Testing
I used jest for testing components that contain some kind of logic.
Also used supertest to test the server endpoints.
You can see the tests in the folders named "__test__" that can be found inside
some folders.

### Score
Decided to generate a string similarity score and a distance score and apply
weight to each of those.
- To generate the city name score I used the Sorensen-Dice coefficient algorithm
- To generate the distance score I calculated the distance between each city and
  the client's geo coordinates and then normalized the values from 0 to 1.

### Searching
The suggestions are queried through elasticsearch.

- I noticed that all of the cities on the doc have population above 5000 so I
  didn't have to add any additional logic to filter the results

### config
Created a configuration file that contains all the config values that can be
overriden by using environment variables.

### Mitigations to handle high level of traffic
I decided to use:
- cluster module: createsan instance of the server for each of the available cpu
  cores. If any of the instances crash it will get respawn.
- Elasticsearch: Is much better than loading the data on memory and it's a very
  fast document store. It also supports replication which I am not using because
  I am using a free basic version of bonsai.io

####Suggestions:
- Using a load balancer would allow us to have n number of instances and it would
  distribute the traffic among them
- Using sharding and replica sets in Elasticsearch.

### additional features
You can additionally pass the "autodetect=1" parameter to the request. If it's
set to 1 then the server will use the geo-ip lite package to get the client's
approximate geo coordinates and use them to score the suggestions.

### Data migration
The data migration script can be found the db migration folder. It utilizes
other modules/packages like config, db, tsv and elasticsearch.
An array is put together with all the parsed values from the tsv file
and injected to elasticsearch by using the bulk method.

To run it just type
```
node db-migration/init-db.js
```

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

### Setting up a Nodejs environment

Get started by installing [nodejs](http://www.nodejs.org).

For OS X users, use [Homebrew](http://brew.sh) and `brew install nvm`

Once that's done, from the project directory, run

```
nvm use
```

### Setting up the project

In the project directory run

```
npm install
```

### Running the tests

The test suite can be run with

```
npm test
```

### Starting the application

To start a local server run

```
npm start
```

which should produce output similar to

```
server started on port 8080
```
