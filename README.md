# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Details

The API at /suggestions is powered by 2 in-memory data structures. 
Namely, a Trie (see: http://en.wikipedia.org/wiki/Trie), and a Map 
(see Data Persistence section).

When the API is called, it simply searches the Trie for all
cities with the given query as a prefix (or whole word)

Based on the list of possible cities returned by the Trie, 
we look up each of those city's information from our Map

With each city's information, we calculate a score (see: Scoring)
and then return the sorted listed of cities along with their scores
as described in the Requirements section.

API is available at https://mitchcity.herokuapp.com/suggestions
Play around with the API via the web app at https://mitchcity.herokuapp.com

## Scoring

Lacking a Latitude and Longitude in the user's query, the score is
simply the ratio of query word length to suggestion word length.
Since we know by the Trie's prefix matching that all the characters 
are the same anyway, all that needs to be taken into account is the length.

Given both Latitude & Longitude, we take a weighted average of our
previous word length score, and a new distance based score. The weight
of the word length is 10%, while distance is 90% as it gives a much more
meaningful idea of what the user is searching for.

The distance based score is the ratio distance to furthest_possible_distance.
Where 'distance' is defined as: distance between the query
Latitude/Longitude point, and the Latitude/Longitude point of the 
suggestion
And furthest_possible_distance is half the Earth's circumference

## Data Persistence

A one time-run script in scripts/, 'create_city_info.js' converts the TSV data
into JSON format. This is the Map discussed in the Details section above.
city_info is in the following format:
```
city_info = {
    city_name: [ // this key is formatted to be all lowercase and with no diacritics
        {
            name: city_name, // this is the full, properly cased, with-diacritics version
            lat: latitude,
            long: longitude,
            country: country_code
            admin1: admin_code
        },
        ...
    ],
    ...
}
```
Each city_name keys a list of cities with that name. Each city in that list has
details which differentiates it from other cities with the same name.

It was decided that this project is small enough that the data structures can
live in-memory. With a much larger dataset, we would need to reevaluate that
assumption and possibly store city_info in MongoDB or something similar.

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
