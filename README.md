# Busbud Back-end Coding Challenge

The BusBud back-end programming challenge

This app is a simple back-end endpoint that serves a list of suggested cities based on a query string. The computed result is a success flag and a list of cities, with their latitudes, longitudes and a score to determine the level of certainty of the match. The app can be accessed at the following url: `http://busbud-backend.herokuapp.com/suggestions?q=`. Optionally the URL accepts `longitude` and `latitude` parameters to refine the search, increasing the scores of cities closer to the desired coordinates. The application passes all given functional tests.

## Getting Started, locally

You can access the API remotely at `http://busbud-backend.herokuapp.com/suggestions?q=`.

If you want to run it locally:

    git clone git@github.com:mac-adam-chaieb/coding-challenge-frontend-a.git
    cd coding-challenge-frontend-a
    nvm use
    npm install
    npm start

Make sure your environment is set up, as described below.


### Setting up a Nodejs environment

Get started by installing [nodejs](http://www.nodejs.org) and [nvm](https://github.com/creationix/nvm).

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
