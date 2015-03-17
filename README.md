# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

##Installation

-Make sure you have mongo and redis installed on your system
-Becasue yes I used MongoDB but not I can't deploy it to Heroku because I cannot afford 19$ per month for an instance on MongoHQ. Sad i know. But thankfully I've included 2 easy scripts to set up easy peasy on your machine!
  -clone this repo
  -cd into the project
  -to install as usual
  ```
  npm install
  ```
  -to populate the mongoDB(this might take a few seconds). This creates a db called 'location-db' with a collection called 'locations'.
  ```
  sh data/import.sh data/cities-canada_usa.tsv data/fieldfile.txt
  ```
  then
  ```
  mongo data/createIndex.js
  ```

  -finally to run
  ```
  npm start
  ```

##Design
Why I used mongo and not streams or other databases?

-1st) Mongo implements full text search on an index of your choice (a bit like Elasticsearch). However, as I sooned discovered, it uses Snowball word stemmer and hence does not match on partial words. This was a real shame because it even provided text-scoring using the $meta expression. So half way through this challenge I decided to simply use $match instead of trying to make it work. The good part is that $match allows to match prefix ascii on the index.

-2nd) Mongo however is real nifty for geoJSON data and also supports indexing on those points. It automatically calculates the distance from 1 point to another and sorts the results by smallest to highest distance calculated. This was perfect because it meant automatic scoring for the distance.

-3rd) Mongo is perfect for json data which is what we were dealing with.

-4th) It matches well with redis, which I used for caching.

##Architecture

-Pretty straight forward:
	- queries are sent to suggestion/
	- they get passed to the locations.js module which has a search(queryString) method
	- if the query is already in redis, the result is returned immidiately
	- a prefex regex is generated for the city name (if the parameter is present)
	- the longitude/latitude and regex is sent to the aggregateBuilder() which matches for results
	- results are returned and stored in redis 
-I didn't go crazy on the scoring and since I prefixed matched the names, we're always pretty confident that what mongo returns is a good match. 
-For the distance scoring since a value [0,1] was needed, I simply took 1 - ( 0.1 / 100 km). Results less than 0 are discarded.
-You can specify a limit in the query string (&limit) to get the top (limit) documents. Default is 20.
	
##Notes

*note 1: the mongodb aggregate doesn't support the full power and options of mapReduce. For this reason it's almost impossible to compute name scores within the aggregate (not many $str options are available). It would have been nice to simply compute the scores and filter them inside the aggregate but alas, it was not meant to be!

*note 2: I tried to do some fancy spelling error corrections using regex, and it worked pretty well. But sometimes it would return data that didn't really have to do with the query. Hondo, TX shouldn't be mistaken for London, ON. Because man if you end up in Hondo...watch out! I think it'd just be better to use something suited for the task than a plain regex.






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
