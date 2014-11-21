# Busbud Coding Challenge (pzn edition)

### Requirements

Fork of https://github.com/busbud/coding-challenge-backend-c.

- NodeJS
- MongoDB (with cities_canada-usa.tsv imported through mongoimport CLI, onto: database geolocationapi, collection cities (default values)

This edition is deployed on Heroku:
- http://pzn.herokuapp.com (web-app)
- http://pzn.herokuapp.com/suggestions?q=montreal (suggestions endpoint)

### Setting up a NodeJS environment

Get started by installing [nodejs](http://www.nodejs.org).

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
npm start
```
