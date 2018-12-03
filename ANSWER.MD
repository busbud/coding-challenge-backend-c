# Overview

The project is deployed on heroku here: https://busbud-suggestions-tclain.herokuapp.com

A two step approach has been adopted to answer the requirements. The data file is generated ahead of time with the command `npm run index:data` and then queried by the api

Of course in a real world setup, the data would come from a database or a search server.

In the database generation phase the process is the following (see `scripts/indexData.js` and `utils/cities.js`):

- get command line params
- load the tsv file at the path given by the user
- going through each cities, retrieving data and generating a canonical version of the name (predictable and without any non significatnt chars, as \_/- and white spaces, see `utils/text => sanititzeString`)

Basically, the process is the following:

- receive a get requests on /suggestions
- validate get params and return an error code if the query is not present OR the coordinates of the reference point is malformed.
- convert the query to a canonical version
- search inside the database of cities for every cities matching the start of the query
- assign a weight to each results matching the levenstein distance of the string => 1 differenct chars on 10 total gives a 90% score (`utils/suggestions/index.js => withLevensteinDistanceScore`)
- if a reference point is given (`utils/suggestions/index.js => weighWithGeoDistanceScore`):
  - go through each matched results,
  - compute the distance in km to the reference point
  - sort by ascending distance
  - compute a geo score, based on the lowest, highest and current distance (a simple liner scale in fact)
  - weighing the text based score with the geo ones. The weight is configurable and is 2:1 ratio for now
- return the results or a 404 if no data

The high-traffic mitigation is handled by a standard rate-limiter.

# Testing

`npm test` will run the tests. A pragmatic approach is used here: 100% coverage is not the only goal, but rather smarts tests are favored for the main features.

# Deployment

`git push heroku master`

# Performance

## Full list search

`npm run benchmark:prod`

the test case consists of 2500 requests, from 50 virtual users, about 100 RPS.

- take 1, integral list search, without rate limiting:

```
All virtual users finished
Summary report @ 01:13:40(-0500) 2018-12-02
  Scenarios launched:  50
  Scenarios completed: 50
  Requests completed:  2500
  RPS sent: 104.21
  Request latency:
    min: 37.3
    max: 8643.5
    median: 239.9
    p95: 674.1
    p99: 1765.4
  Scenario counts:
    0: 50 (100%)
  Codes:
    200: 2500
```

- the current benchmark will be rate limited and use caching, reducing the max time by 6x.

```
Summary report @ 08:55:12(-0500) 2018-12-03
  Scenarios launched:  50
  Scenarios completed: 50
  Requests completed:  2500
  RPS sent: 55.51
  Request latency:
    min: 45.7
    max: 1572.4
    median: 111.7
    p95: 304.2
    p99: 666.3
  Scenario counts:
    0: 50 (100%)
  Codes:
    200: 2500

Done in 46.09s.

```

# Gotchas

- Readable province and states names come from: 'http://www.geonames.org/CA/administrative-division-canada.html'

To reduce time and focus on businnes logic, the usual `express` and `lodash` has been used.

For tests, `jest` has been favored instead of `mocha` + `chai` + `istanbul`, for its abilities to being used in the backend, frontend (very suited for react apps) and the all-in-one included philosophy + (snaphots, yeah)

- base functionnal tests were not fully robust, so have been updated to jest + fixed (for instance the lat, lon assertions did not take in account type coercions of empty strings)

The LTS version of node at this time has been used, 0.10.26 being vulnerable to multiple attacks:
https://www.cvedetails.com/vulnerability-list/vendor_id-12113/product_id-30764/version_id-192865/Nodejs-Node.js-0.10.26.html

## Next steps in real-life

- cache !
- Add type system like typescript
- Use a true index to descrease numbers of array trasversal
