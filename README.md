# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

This task was requested in order to demonstrate JavaScript coding skills and ease of solving problems. As the title explains, this is a coding
challenge for Busbud (www.busbud.com).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Libraries Used](#libraries-used)
- [Design Decisions](#design-decisions)
  - [Algolia's Search Engine (https://www.algolia.com/)](#algolias-search-engine-httpswwwalgoliacom)
  - [Others](#others)
- [Install Instructions](#install-instructions)
  - [Preparing Algolia Search Engine](#preparing-algolia-search-engine)
  - [Installing API](#installing-api)
- [Features](#features)
- [Caveats](#caveats)
- [Improvements](#improvements)
- [Requirements](#requirements)
    - [Sample responses](#sample-responses)
  - [Non-functional](#non-functional)
  - [References](#references)
- [Getting Started](#getting-started)
  - [Setting up a Nodejs environment](#setting-up-a-nodejs-environment)
  - [Setting up the project](#setting-up-the-project)
  - [Running the tests](#running-the-tests)
  - [Starting the application](#starting-the-application)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Libraries Used

The following libraries were used:

- "algoliasearch": "^3.7.8"
- "algoliasearch-helper": "^2.3.4"
- "express": "^4.13.3"
- "cool-ascii-faces": "~1.3.x"
- "fuzzysearch-js": "^0.1.1"

## Design Decisions

### Algolia's Search Engine (https://www.algolia.com/)

Since the objective of this challenge was to create an API that could handle heavy transactions, I reasoned that an engine allowing changes
of number of hits per page shown, indexing attributes, etc. was most appropriate. In this case, Algolia allows changes to be done to the
engine and index at any time through their UI instead of doing extra modifications to the API. Since it is Saas based, the user of the
provider of the API should add their own keys in the right location of the API.

### Others

- Express (http://expressjs.com/):
    - Provide structure to my api
    - Ease routing in the api
    - Ease parsing of queries

- AlgoliaSearch-Helper (https://github.com/algolia/algoliasearch-helper-js):
    - Allow faster querying
    - Access a ready to go client of Algolia

- FuzzySearch-JS (https://www.npmjs.com/package/fuzzysearch-js):
    - Allow for more accurate auto-complete
    - Provide a scoring system for suggestions

## Install Instructions

### Preparing Algolia Search Engine

The data of the cities that need to be suggested are stored in TSV (Tab Separated Values) format. In order to
have Algolia's search engine index this data one must converts the value to JSON format.
To do so, install tsv-to-json (https://www.npmjs.com/package/tsv-to-json):

    npm install tsv-to-json

To use:

    node {$tsv-to-json_dir}/bin/tsv-to-json $orig_file.tsv $dest_file.json

Then follow Algolia's starting guide to upload the index: https://www.algolia.com/doc/javascript#using-the-api

### Installing API

This API is based on node.js, to get familiar with setting the environment please read [Getting Started](#getting-started) below.
At this point your API should be ready to go locally.

## Features

There are parameters that you may play with in order to manage your experience with the accuracy of results:

    - `h` is hits per page. This is the numbers of objects retrieved from the search engine. Default is 20.
    - `g` is geolocation precision. This is the radius in kilometers of the geo search based on the latitude and longitude provided. Default is 50KM.

To return 30 objects within a 100KM radius do as follow:

    ex: GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163&g=100&h=30


## Caveats

- The AlgoliaSearchHelper returns a JavaScript objet in this format: https://github.com/algolia/algoliasearch-helper-js#results-format
- Algolia vs Solr: http://stackoverflow.com/questions/26484394/algolia-vs-solr-search
- Elasticsearch vs Algolia: http://www.quora.com/How-does-Elasticsearch-relate-and-or-compare-to-Algolias-Search-as-a-Service
- Administrative divisions:
    - USA: http://www.geonames.org/US/administrative-division-united-states.html
    - Canada: http://www.geonames.org/CA/administrative-division-canada.html
- Geolocation engine: http://www.movable-type.co.uk/scripts/latlong.html

## Improvements

- There are some cities that are duplicate in the data source used ex: Monticello,KY appears twice in geonames due to timezone.
- The data source could have a field named _geoloc in order to perform the geolocation search inside the search engine instead of in the server.

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