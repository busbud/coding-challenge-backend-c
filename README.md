# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Work accomplished

- Add Gulp to have a livereload on dev environment
- Add a script (convertJson.js at root folder) to generate a json file for cities data from the dump of geonames.org or data/cities_canada-usa.tsv file
- Add logic to retrieve matching cities with latitude, longitude and a score. Score is calculated by the percentage of the request variable length compared to the full name city if there are no latitude and longitude submitted. If latitude and longitude are submitted, score is calculated according to the distance from the user.

## Commands

- `npm i` : Only once on the first time to install libraries dependancies
- `npm i -g gulp` : Only once on the first time, Gulp seems to need be installed globally
- `npm run dump` : to build json file for cities data from the dump of geonames.org or the one furnished on the test (Only once or every day combined with an automated task to download new source file daily for example)
- `gulp` : launch development server on [http://localhost:2345](http://localhost:2345) with livereload On
- `npm start` : launch server available on [http://localhost:2345](http://localhost:2345)
- `npm test` : to execute unit test that must pass

## Tools Used

- lodash for useful functions
- (dev) gulp and gulp-livereload to have a live reload on development environment
- (dev) jsonfile and line-reader to parse cities data from a source file in the dump script

## Heroku Url

[Website on Heroku](https://desolate-river-12046.herokuapp.com/suggestions)
Ex: [With partial text londo](https://desolate-river-12046.herokuapp.com/suggestions/?q=londo)

## Mitigations to handle high levels of traffic

- We could use PM2 with a cluster system to handle high traffic
- In addition of PM2, we could deploy this app using Docker behind a load balancer, scaled with Kubernetes (Kubernetes replicate as many instance as demand is high and close them when demand get lower) as in schema below:
![Infrastructure](/docs/docker-kubernetes.png)

## Remarks

- Add `robots.txt`, `favicon.ico` files and a method to serve those static files to prevent Heroku app to crash
