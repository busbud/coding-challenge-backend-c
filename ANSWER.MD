# TODO

[ ] sort index for max reuse
[ ] sanitiez user input and indexing phase on name
[ ] add tests
[ ] support geodistance sorting
[ ] support and benchmark from index

# Testing

`npm test` will run the tests. A pragmatic approach is used here: 100% coverage is not the only goal, but rather smarts tests are favored or the main features.

# Deployment

The project is deployed on heroku here: https://busbud-suggestions-tclain.herokuapp.com

`git push heroku master`

# Performance

## Full list search

`npm run benchmark:prod`

the test case consists of 2500 requests, from 50 virtual users, about 100 RPS.

- take 1, integral list search, without any optimizations:

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

# Gotchas

To reduce time and focus on businnes logic, the usual `express` and `lodash` has been used.

For tests, `jest` has been favored instead
of `mocha` + `chai` + `istanbul`, for its ability to been used in the backend, frontend (very suited for react apps) and the all-in-one included philosophy + (snaphots, yeah)

- base functionnal tests were not fully robust, so have been updated to jest + fixed (for instance the lat, lon assertions did not take in account type coercions of empty strings)

The LTS version of node at this time has been use, 0.10.26 being vulnerable to multiple attacks:
https://www.cvedetails.com/vulnerability-list/vendor_id-12113/product_id-30764/version_id-192865/Nodejs-Node.js-0.10.26.html
