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

Then, run

```
sqlite3 data/geonames.db < setup_database.txt
```

which creates the database and loads it with data

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



# Implementation

### Parameters

The suggestions API has five parameters that can be easily changed, without requiring an understanding of the implementation. These are found at the begining of app.s and are:

General performance parameters:
- MAX_SUGGESTIONS: the number of suggestions being returned to the user. Default value is 5.
- RESPONSE_TIMEOUT: defines the maximum amount of time the server will spend processing an API request. If reached, an empty suggestions array is returned in JSON. Default value is 2000ms.

Scoring algorithm parameters:
- DISTANCE_WEIGHT: defines the weight of distance in the scoring algorithm when user location is provided, out of 10. Default value is 4.
- SMOOTHING_DIST: smoothing factor applied to the distance dependant component of the scoring algorithm, used to reduce score amplitude between close and far cities. Smaller factor leads to reduced amplitude. Default is 0.5.
- SMOOTHING_POP: same as above, but for the population dependent component, and reduces score amplitude between large and small cities. Default is 0.5.

### Scoring Algorithm

The scoring algorithm used is:

#### Without user location:
````
score = score_pop = (city_population/largest_population_of_matching_cities)ˆSMOOTHING_POP
```

#### With user location:
The location based component of the score is computed as follows:
````
score_dist = (smallest_distance_of_matching_cities/city_distance)ˆSMOOTHING_DIST
`````
And the final score is:
`````
score = [ score_dist * DISTANCE_WEIGHT + score_pop * (10 - DISTANCE_WEIGHT) ]/ 10
````

### High level of traffic mitigations

The following steps have been undertaken to mitigate performance loss when handling large amounts of requests.

1. Limit the number of suggestions to 5.
2. Return only the 5 biggest matching cities from the database, when the user's location is not provided.
3. Impose a time limit for handling each request, after which it is gracefully aborted.

While (1) only affects the duration of the response data transmission, it enables (2), which in some cases may drastically reduce the quantity of data being manipualted by the program and thus improve its response time.

Finally, (3) will have the benefit of reducing the load on the server when under high pressure by limiting the duration of each request. This is without loss of functionality because auto-complete requests need to work in real-time, as a suggestion coming in with a delay greater than a few seconds is very likely to have become irrelevant by the time it reaches the user.



