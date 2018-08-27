# Busbud Coding Challenge

## About

This application uses Node.js, PostgreSQL (with the PostGIS extension) and Redis to serve a text and location-based autocomplete search service.

The application uses Express to serve a single `/autocomplete` endpoint for all results. The base environment comes with a docker-compose configuration for getting started quickly, otherwise the latest LTS version of Node.js can be installed locally. In development, I use docker-compose as well as ESLint and Flow. Tests can be executed using `docker-compose -f docker-compose.test.yml up`.

City data is stored in a PostgreSQL database (automated using the script at `scripts/importer.js`). I decided to use PostgreSQL because of its familiarity and also because I've used PostGIS in the past for other projects. While I didn't end up using Postgres to filter or score results (as was my initial plan), PostGIS is still very handy for getting accurate distance results when the user's position is sent in the querystring.

The scoring algorithm assesses four criteria: prefix matches, substring matches, distance and population. Each are weighted slightly differently to provide a decent blend of results. **Prefix matches** make the application highly prefer full prefixes (e.g. "lon" gets you London or Longueuil first). This is a very common autocomplete method. To help cover typos, misspellings and other edge cases, **substring matches** are also considered (using the [metric-lcs library](https://www.npmjs.com/package/metric-lcs)). For this reason, even if the user searches for "longeuil", Longueuil will be the highest-ranked result. Finally, both **distance** from the search latitude/longitude (if provided) and the **population** of each city will have a certain weight in their search ranking. It's not a perfect system but it seems to handle most cases well!

To help cope with large amounts of traffic, a Redis instance is used for caching purposes. Requests are cached based on query string used, and (if present) the latitude / longitude of the user to the 1st decimal place. For example, if a user in downtown Montréal searches "lon", their results will be cached, so that the next person searching "lon" in downtown Montréal will get their results directly from Redis, resulting in a roughly 10x speed improvement for frequent queries. Results are currently cached for 5 minutes.


## Possible Improvements

- Improve the string matching part of the search algorithm (don't weigh missing characters so highly)
- Add swappable ranking parameters (make it possible to specify rank weights by city or search length)
- Save and interpret usage data and amplify the ranking of the most commonly selected/shared results per metro area.
- Use machine learning to anticipate the most likely possible next keystrokes in the search query and suggest these more precise results in advance.
- Improve test coverage and test for the dependability of results (e.g. make sure that when you search "lon" from Montréal, you get London, Ontario first in the results).
- Add continuous integration and deployment.


## Original Requirements

Design an API endpoint that provides auto-complete suggestions for large cities.
The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

- the endpoint is exposed at `/suggestions` ✅
- the partial (or complete) search term is passed as a querystring parameter `q` ✅
- the caller's location can optionally be supplied via querystring parameters `latitude` and `longitude` to help improve relative scores ✅
- the endpoint returns a JSON response with an array of scored suggested matches
    - the suggestions are sorted by descending score ✅
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident) ✅
    - each suggestion has a name which can be used to disambiguate between similarly named locations ✅
    - each suggestion has a latitude and longitude ✅
- all functional tests should pass (additional tests may be implemented as necessary). ✅
- the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs). ✅
- feel free to add more features if you like! ✅

### Non-functional

- All code should be written in Javascript ✅
- Mitigations to handle high levels of traffic should be implemented ✅
- Challenge is submitted as pull request against this repo ([fork it](https://help.github.com/articles/fork-a-repo/) and [create a pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)). ✅
- Documentation and maintainability is a plus ✅


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
