# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Implementation

Endpoint available here:
https://busbud-backend-challenge.herokuapp.com/suggestions

Search 'lon' from Montreal: 
https://busbud-backend-challenge.herokuapp.com/suggestions?q=lon&latitude=45.5017&longitude=-73.5673


#### Approaches and algorithm to achieve autocomplete suggestions
- Dedicated data structures such as tries
- Tokenizing (n-grams)
- Similarity algorithm or string distance. Most common ones: Levenshtein, Jaro-Winkler, Soundex, q-grams, Jaccard
 
#### Some considerations
- match substrings as well?
- support fuzzy searching?
- how is confidence being determined? similarity, geolocation?
- where is the data currently stored? chosen data store (PostgreSQL, Redis, Elasticsearch...) will include built-in features 
that will define the granularity of our code.

#### Current implementation 

This implementation uses Elasticsearch. Having a separate data store generates management, machine resources and maintenance 
but quite a valuable approach in the case of a static set of data like cities. 

Implemented features: 
- geolocation based scoring
- fuzzy search   

Not implemented: 
- Support for alternate names but can be done with ease. 

Pros: less code to write, easy queries

Cons: scores are finicky to work with, not easy to understand and can't be set between 0 and 1 (which doesn't meet the requirements here!)

Autocomplete suggestions is achieved using the [index-time search-as-you-type](https://www.elastic.co/guide/en/elasticsearch/guide/master/_index_time_search_as_you_type.html) configuration
paired with a [prefix query](https://www.elastic.co/guide/en/elasticsearch/guide/master/prefix-query.html) to give more importance to the beginning of the city name.

Note: PostgreSQL is surely just as good for this use case => support for fuzzy searching and similarity ranking.  
No support for geolocation natively (though it seems to be possible using the PostGIS extension), but could be done in the code as a post query step.

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
PORT=3456 npm start
```

which should produce output similar to

```
Server running at http://127.0.0.1:3456/suggestions
```
