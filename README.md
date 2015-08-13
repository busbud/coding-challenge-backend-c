# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Requirements

Design an API endpoint that provides auto-complete suggestions for large cities.
The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

- The endpoint is exposed at `/suggestions`.
- The partial (or complete) search term is passed as a querystring parameter `q`.
- The caller's location can optionally be supplied via querystring parameters `latitude` and `longitude` to help improve relative scores.
- The endpoint returns a JSON response with an array of scored suggested matches.
    - The suggestions are sorted by descending score.
    - Each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident).
    - Each suggestion has a name which can be used to disambiguate between similarly named locations.
    - Each suggestion has a latitude and longitude.
- All functional tests should pass (additional tests may be implemented as necessary).
- The final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).
- Feel free to add more features if you like!

#### Sample responses

These responses are meant to provide guidance. The exact values can vary based on the data source and scoring algorithm.

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

- All code should be written in JavaScript.
- Mitigations to handle high levels of traffic should be implemented.
- Work should be submitted as a pull-request to this repo.
- Documentation and maintainability is a plus.

### References

- GeoNames provides city lists Canada and the USA: http://download.geonames.org/export/dump/readme.txt
- http://www.nodejs.org/
- http://ejohn.org/blog/node-js-stream-playground/

## Getting Started

Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and
[Windows](http://windows.github.com/) that make this easier.

### Setting up a Node.js environment

Get started by installing [Node.js](http://www.nodejs.org).

For OS X users, use [Homebrew](http://brew.sh) and `brew install nvm`.

Once that's done, from the project directory, run:

```
nvm use
```

### Setting up the project

In the project directory run:

```
npm install
```

### Running the tests

The test suite can be run with:

```
npm test
```

### Starting the application

To start a local server run:

```
PORT=3456 npm start
```

Which should produce output similar to:

```
Server running at http://127.0.0.1:3456/suggestions
```
