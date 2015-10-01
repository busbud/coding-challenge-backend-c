# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Chris Kahn's Solution

https://obscure-sands-3627.herokuapp.com/

My solution consists of three parts. First there is a script that will parse the TSV file and stream the relevant
data directly into a MongoDB instance. Then I used expressjs to serve up the suggestions API endpoint, which retrieves
results from Mongo, as well as a simple frontend gui based on React.js. For data storage I used the promised-mongo
library, a rewrite of the mongodb interface to directly return promises rather than using callbacks.

I believe this setup should do sufficiently well with the task at hand. I did not want to over-complicate the challenge
with something like elastic search. I settled on storing the city names, together with their state/province and
 country in the ascii forms with diacritics removed, and doing a simple match against this. To create a canonical
 name of each location I used each component of the location separated by commas like so:

    mission,ks,us
    mission,tx,us

I also realized that MongoDB has built-in capabilities to perform geographic proximity searches, and I decided to
let MongoDB handle this aspect of the algorithm. Since Mongo will return the results sorted nearest-to-furthest, I
 simply use the ordering of the results to apply a closeness factor to the calculated score. On Heroku's free level
 this significantly slows down queries, however.

For the string comparison itself, I chose to use the Jaro-Winkler Distance, which should be faster than Levenshtein
while producing a similarity score in our scale from 0 to 1. We can then massage this score based on location as
mentioned above, full or partial match, etc.

Traffic mitigation is done in two parts: first, I used a library called 'toobusy' which polls the event loop and,
if the server is too busy, returns a 503 Unavailable so that we don't overload the server and slow down *everyone's*
requests. Secondly, we limit the number of results we will accept from the database. Thirdly, I added the option to
send a "limit" value with the request so that the client can limit the amount of traffic they want to receive. If the client
only needs 10 items, it should request 10.

I also fixed what appears to be a copy-paste error in the last test case: it checks for the presence of the location
fields when it should actually be looking for the score field.

## Organization

I organized the app in a style similar to what I'm used to in Scala. There is a repository layer responsible for
communicating with the database, a service layer which implements our domain logic (scoring algorithm), and a
controller layer which handles the request/response. Dependencies are passed in through constructor parameters allowing
us to pass in mocks in unit testing, or for example pass in different repositories based on the database type
configured. This separation of concerns makes it easier to maintain and expand upon.

## Setup

I like to use Makefiles to help make managing all the different commands easier.

    make deps

    # Runs the import script against the local MongoDB instance
    make import

    # Starts `gulp watch` on the frontend assets, and nodemon to run the server.
    make run

However these basically make calls to npm or gulp:

    gulp watch

    gulp compile

    babel-node ./data/importCities.js

    npm start

    npm test

## Future Improvements

I noticed that performing a location search significantly slows down the API's response time. Perhaps a caching layer,
to store commonly searched terms could speed up results. Alternatively, there could be a faster way (or shortcut) of
comparing distance than what MongoDB is doing. I also need way to find typos or misspelled city names.


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
