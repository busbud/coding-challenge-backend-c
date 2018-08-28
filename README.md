# Busbud Coding Challenge [![Build Status](https://travis-ci.org/SamuelBolduc/coding-challenge-backend-c.svg?branch=master)](https://travis-ci.org/SamuelBolduc/coding-challenge-backend-c) [![Coverage Status](https://coveralls.io/repos/github/SamuelBolduc/coding-challenge-backend-c/badge.svg?branch=master)](https://coveralls.io/github/SamuelBolduc/coding-challenge-backend-c?branch=master)

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

## My implementation

An `express` server handles the queries. The structure is a bit overkill for a single endpoint, but it's what I usually use and it works well. All the API endpoints are handled in `routes` and they usually call `models` to interact with the database.

In this case, the database is simple an in-memory representation of the parsed data. The data is parsed when the server starts, and the server only starts listening for connections when the data has been fully loaded and parsed. In a real application, I would store this data in an SQL database (PostgreSQL would be my go-to choice).

For logging, I tried a lot of logging libraries over time but I always come back to `log4js`. Since I always ended up overriding some methods and settings, I wrote my own wrapper, [log4js-wrapper](https://www.npmjs.com/package/log4js-wrapper), that I use in all my projects.

### Testing

I tried to get as much code covered as I could. The remaining untested code (specific branches in specific files) are very hard and time consuming to test.

I adapted the existing `supertest` tests to use async / await, and also added some to cover more cases (like foreign languages).

`nyc` is used to generate coverage statistics (uses `istantul` behind the scenes). Tests and coverage reports are automated using Travis CI and coverage data is pushed to coveralls.io.

`pre-commit` is used to run the tests before every commit. The `npm test` command also makes sure there's no `eslint` warning or error.

### Caching

Redis is used to cache the query results. It drastically improves performance when a request is cached. I only cache the results before distance score is applied. In cases where the server handles a lot of traffic, a lot of queries are likely to be repeated, so this will vastly improve performance.

### Scoring

I've spent a lot of time fine-tuning the scoring algorithm. It is influenced on 4 criteria:

- Prefix: a matching prefix will give a score of 1 to this criteria. If it doesn't match, alt_name will be scanned and if a match is found there, a score of 0.2 will be given to that city. I set it so low because a lot of times, many alt_names are specified and do not look at all like that actual city name, so it looks a bit buggy if these for example Boston is a top result when you search for "ga";
- String: using `string-score`, calculate the match of the string. It is especially useful for longer names where the user is more likely to have written the complete name of a city when typos. The fuzzyness (degree of tolerance) of the function is greater for longer searches and lower for shorter searches (so the first few keystrokes really show cities starting with those letters instead of cities that have a similar name);
- Population: a city's population score is its population ratio relatively to the most populous cities in the matches array, since bus rides are more likely to be between bigger cities;
- Distance: the distance score is calculated the same way as the population score. I tried calculating them relatively to their index in the array instead of relatively to the shortest distance, but it gave some bad results in some instance where a lot of cities are super far.

The final score is, I think, very accurate. I tried a lot of searches and it surprised me how pertinent the results were. 

It's also possible to search for cities in another type of writing, for example in cyrillic. Not all cities have a lot of `alt_name` entries but the most important ones do.

Weights are applied to each score component and easily editable in the `config/default.yml` file.
