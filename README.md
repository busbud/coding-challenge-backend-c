# Busbud Coding Challenge - Dustin Blackman [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Design

This application uses MongoDB as a backend to store search results. Initially the plan was to use Mongos text index, only to later find out it didn't support partial searching. After doing some tests I found that a combination of a regular string index with a regex query was super fast, I can't complain.

Mongo also supports geospatial searches out of the box, so integrating the ability to sort by location was fast and simple.

Data in the database is minimal as I only took the parts I needed from the TSV to keep database size down. Were querying the *phrase* key, which has had all accents removed from it allowing for easy queries. Here's an example document.

```
{
  "phrase": "montreal qc ca",
  "city": "Montréal",
  "lngLat": {
    "type": "Point",
    "coordinates": [
      -73.587810000000004607,
      45.508839999999999293
    ]
  },
  "country": "CA",
  "state": "QC"
}
```

The reason I kept city, state, and country separate was to make it easy to use these keys in different ways in the future. And why I didn't create a key with all of them combined was to keep database size down (even if it's just a little bit).

As queries can get quite large, there's a limit set in the query to Mongo to prevent more than 5000 results to come in. I understand the TSV provided only has ~7000 documents, but I wanted to futureproof a bit.

## How It Works

There's a frontend available to get a feel of how the application works without looking at code or JSON responses. Currently the frontend requires a minimum of 2 characters and executes search 200ms after last key stroke. You can find it here! http://dblackman-busbud.herokuapp.com

/suggestions accepts 4 parameters
- q (query text)
- latitude
- longitude
- limit

Example:
```
/suggestions?q=mon&latitude=45.508669900000000000&longitude=-73.553992499999990000&limit=5
```

At request, Nodes backend checks for what parameters have been supplied. If Q is missing, it returns error. If limit is available, results will be as well.

Next is building the query. Were comparing to the *phrase* key in Mongo using Regex and comparing to the beginning of the string. If Lat/Lon is supplied, Mongo's $near is added to the query object.

For score, initially a score is created based on string compare from 0 to 1, so the closest string match will have a greater score.

If however LatLon is supplied and $near was added to the query, we want the nearest results to come up first with a combination of string score. As Mongo returns the results nearest/farthest, we reverse the results array so the nearest result is at the end, then using an array index, we multiply each entry by a small float to increase each entry based on distance (which is it's position in the array). Afterwards, this gets sent to the sorting function, which results in the nearest location coming up first! See the code for yourself [here](./functions/queryMongo.js#L70)

## Notes

1. MongoLab I'm realizing is a bit slow, especially on the first request if no other request has been made to the DB recently. To see true potential, follow the steps below to setup a local instance.

2. As the *name* key that is __returned__ has accents for cities like Montréal, the tests were failing as it was comparing to Montreal to Montréal. I changed the tests to Toronto instead.

## Setup Mongo Locally
1. Setup a Mongo instance with it's default config if you don't have one already, you don't have to do anything special.

2. ```
cd ./data
node tsv2mongo.js
```

3. Open up __./functions/queryMongo.js__ and switch out the comments for the Mongo connection. Should look like this afterwards.

```
var colc = mongo('127.0.0.1:27017/busbud').collection('suggestions');
// var colc = mongo('read:read@ds059651.mongolab.com:59651/heroku_app35517737').collection('suggestions');

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
- Work should be submitted as a pull-request to this repo
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
PORT=3456 npm start
```

which should produce output similar to

```
Server running at http://127.0.0.1:2345/suggestions
```
