# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

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









# :bus: :bus: Busbud Coding Challenge - Implementation notes :bus: :bus:



## Features: 

- The app supports partial and complete search term.

- It supports searching by non english term, because why not? :happy:

- It support searching by term that has space or '-'.



## Heroku URLs:

- [/suggestions?q=Mont](https://busbud-city-suggestions.herokuapp.com/suggestions?q=Mont)

- [/suggestions?q=Montreal](https://busbud-city-suggestions.herokuapp.com/suggestions?q=Montreal)

- [/suggestions?q=Montréal](https://busbud-city-suggestions.herokuapp.com/suggestions?q=Montréal)

- [/suggestions?q=Montreal&latitude=45.50884&longitude=-73.58781](https://busbud-city-suggestions.herokuapp.com/suggestions?q=Montreal&latitude=45.50884&longitude=-73.58781)

- [/suggestions?q=蒙特利尔](https://busbud-city-suggestions.herokuapp.com/suggestions?q=蒙特利尔)

- [/suggestions?q=蒙特](https://busbud-city-suggestions.herokuapp.com/suggestions?q=蒙特)

- [/suggestions?q=مونتریال](https://busbud-city-suggestions.herokuapp.com/suggestions?q=مونتریال)

- [/suggestions?q=مونتر](https://busbud-city-suggestions.herokuapp.com/suggestions?q=مونتر)


## Implementation:

I've implemented the app using express js. It makes writing code easier with middleware, routes etc..

The app is using the data provided in `data/cities_canada-usa.tsv`. The data is being indexed based on these columns: `name`, `ascii`, `alternatate name`. The index in saved into memory for fast suggestion lookup. For data heavy app we would eventualy use Redis or similar distributed cache.



**How index is generated?** 

 For a given city like Montreal with this data: `id:12345, name: Montreal` the index would generate keys like:

```
M ->12345
Mo ->12345
Mon ->12345
Mont ->12345
Montr ->12345
Montre ->12345
Montrea ->12345
Montreal ->12345
```

The process is the same for the `ascii` and `alternate` columns.

Spaces and '-' are removed before creating the index so `Montreal - Ouest` will be processed as `MontrealOuest`



## Configuration :

The app is using configuration file in `config/conig.js`



## Scoring:

I did a very basic and simple scoring for now. It gets the job done but could definitely be improved. 

It compare the length search key with the length of the indexed key. The higher the better. The best is one both length are equal, if so we return `score:1`.

Latitude and longitude are taken into consideration. If we found a city with exact lat/long, we return a `score:1`.



## :rocket: Performance :rocket:

The app is caching request response for faster lookup.  It is using a simple in memory cache that never expire. It should be okay in the context of this challenge. In a real word app, an optimazed version could be to use a LRU cache strategry, Redis or other options.

**Avg. response time with 1000 request/second over 1 minute is 17ms. Please check load test result [here](http://bit.ly/2PJQtME)**

![loader.io](test/loader.io.png)