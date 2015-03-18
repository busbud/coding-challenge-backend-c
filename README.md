# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/sataz/coding-challenge-backend-c/tree/feature%2Fst-challenge.png?circle-token=0e5fe529dd2e8b395a38b24741e3c0041db1056c)](https://circleci.com/gh/sataz/coding-challenge-backend-c)

## Implementation

Here are some details about my implementation:

* the tsv file is loaded once, processed, and necessary data are kept in memory
* I used levenshtein score to calculate the distance between 2 words. I had to invert/convert that score to fit the requirement (score between 0 and 1) and some weird results when the query length was a lot smaller than the city name (ex. "Mtl" score for "Montreal" was really low...). The score adjustment is based on basic Maths rules ("increase the score if query length is smaller than city name"...) and it is empirical.
* for distance score, I used basic Maths and decided to use the square root function because of its curve that corresponds to what I had in mind (boost the score when coordinates are close...).
* the score combination (text and distance) is linear. I decided to give more weight to the text (0.7 vs 0.3) because if the goal is that api in an bus travel application with geolocation, then the user might be planning his next vacation to Costa Rica from Montreal.
* for distance calculation, I chose a simple method because:
    * it's faster,
    * the score is more impacted by the distance when the distance is small, and the simplified method is good enough for small distance

I also started a proof of concept with [ElasticSearch](http://www.elastic.co/) but I didn't get a chance to complete it. More details about this POC [here](#es-poc).

Finally, I quickly built a UI that shows the results on a map in order to test the API.

# API and URL

I built:
* the API that was requested: [check it on heroku](https://sleepy-ridge-3726.herokuapp.com/suggestions?q=Mtl). Paramters:
** q: string (required)
** latitude: float between -90.0 and 90.0
** longitude: float between 180.0 and 180.0

* An API that uses ElasticSearch (not finished): [check it on heroku](https://sleepy-ridge-3726.herokuapp.com/suggestions?q=Mtl). Parameters:
** q: string (required)

* a UI that consumes the v1 API: [check it on heroku](https://sleepy-ridge-3726.herokuapp.com/).


# Modules

THey can be find in lib:

* location-parser: uses node stream to process the tsv and return a list of locations.
* location: a class that represent a city, that contains a name, state, country and a geoPoint
* geo-point: represent a latitude/longitude pair (a coordinate).
* query: repesent a query that a user can type: a text and eventually a geoPoint.
* score: helper function that centralize the text and distance calculation.
* store: memory store that knows how to find suggestions from the list of locations.
* errors: some errors that are used by the previous modules.

## Tests

47+ tests, mostly unit tests, plus the functional tests that I refactored a little bit.


## Modifications

* removed 127.0.0.1 from app.js to make it work on heroku
* fixed one test because a string does not support `test` method
* I limited the suggestions to 5 by default (was not specified I think)
* I allow a `limit` param to change the default limit of 5
* I allow a `theeshold` param to change the default theeshold of 0.15 (not suggested if score is below 0.15)


## Performance

I used jmeter against http heroku (1 dyno). I am new to it, so I hope that I interpreted the results correctly. with 50 users, ramp-up of 5s, loop count of 20. Note that I am executing the same query term during the test which is not realistic.

* with just the q parameter ("Mtl"): I get 5762 requests/s (253ms average)
* with q parameter ("Mtl") and lat/long: I get 4594 requests/s (average 390ms)

Also, I checked the performance of the ES powered API (/v2/suggestions), using `q=Mont`:
* with just the q parameter ("Mtl"): I get 8874 requests/s (78ms average).

Note that with ES, when using the same query parameter,  the request is always longer than subsequent ones.


## Possible Improvements and other considerations

* add a api version, in the URL or the header (and default to latest)
* Add a redis and cache results per query (for example by using a hash of query as key).

## Resources

* [heroku app](https://sleepy-ridge-3726.herokuapp.com/suggestions?q=mtrl&latitude=45.5&longitude=-73.5)
* [github repo](https://github.com/sataz/coding-challenge-backend-c). Note that I used temporary github account for this challenge...


## ES POC

I wanted to see if ES could help us achieve our goal, in a more production-ready way, and with the same or a shorter response time.

So I created some [scripts to import data into ES](./bin/es).
I used Bonsai on heroku (free for 10,000 documents).

I didn't get a chance to finish this Proof of concept due to lack of time.

But I have a new API `/v2/suggestions` that only use `q` parameter.

### ES - What is left

* Tweak ES configuration: index, field types, query type
** Learning ES in 2 hours is a difficult (impossible) challenge.
* Build the right query!
** the right query to return the right result: right now, [q=1](http://sleepy-ridge-3726.herokuapp.com/v2/suggestions?q=Mtl) returns no results.
* return a score and make it a value between 0 and 1. I am not sure if it is possible have ES scores to a 0-1 range directly in ES. Wort case, we can divide all scores by max_score.
** use latitude and longitude query param: I found a way to combine query using AND condition (with ["MUST"](http://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-bool-query.html)),
which would allow '"name is close to Mtl"' AND coord in a range of 200km'
but I didn't find a way yet to change the score based on "geo proximity".
** Currently, [searching for "Mont"](https://sleepy-ridge-3726.herokuapp.com/v2/suggestions?q=Mont) does not return Montreal.
* Test ./lib/es module and add functional tests.

# Conclusion

I would continue investigating ElasticSearch as a replacement solution. [ES cache mechanism](http://www.elastic.co/guide/en/elasticsearch/reference/current/index-modules-shard-query-cache.html) seems powerful and improve the response time. Elasticsearch seems really powerful and comes with a lot of configuration options at many level (index, query, cache...).


(below: original README text)
-------------------------------------

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
