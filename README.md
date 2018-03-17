# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

[Gabidi Heroku Deployment HERE](https://gabidi-bus-bud.herokuapp.com/suggestions?q=lon&latitude=45.5017&longitude=73.5673)

# Environment upgrades 
- Node version in .nvmrc was 0.1 from 2014. Upgraded to v8.10.0
- Typescript transpiled to ES6 for awesome level upgrade and more importantly:
    - Sanity at scale/maintainability
    - Avoid chasing silly time consuming mistakes.
- Intentionally kept app.js simple to avoid having to re-write tests
    

# Notes on Tests
- Added data-parsing.ts : Tests for parsing and processing city tsv data

    - Added tests to make sure score rankings makes sense
    - Modified ./test/suggestions.js line 57: 
      ```js
      // C'est é :p
      // return suggestion.name.test(/montreal/i);
      return /montréal/i.test(suggestion.name)
      ```
# Notes on Functionality Created
- Most work done is in ./lib/
    - *server-helpers.ts* 
        - Class with utilities to take complexity away from app.js, handles throttling and orchestrating data; 
    - *geonames-import.ts* 
        - Handles parsing and constructing trie for city data search; 

### Score Calculation Notes:

-Score calculation done in ./lib/server-helpers.ts >  calcCityScoreFromCord() and getSuggestionsFromRequest() 

-Score Algo:
```js
        let score = 0;
        if (latitude && longitude) {
            // Larger distnace = smaller score
            score = 1e6 / (this.distance(lat, long, latitude, longitude)*2)
        }
        // Final score add emphasis on distance form long/lat and small boost for population
        // i'm sure parameters can use some tuning, but the idea is there
        return score + ((population) ? score*(Math.log(population + 1)) : 0)
```
- Class  ./lib/server-helpers.ts has functions responsible Calculation of scores.
- Distance is most important and give boost to larger (population) cities     
- Notes:
    - This query requires context. For example: If the calculation is being done for a destination selection then really close distances should be penalized as people are unlikley to be booking a bus to go to another borrow.
    - Ideally one would factor in outbound/inbound city frequency into score data to suggest cities people frequently book for given long/lat combo. 

### Throttling Notes
- Throttling done by ./lib/server-helpers.ts >  thottleConnection()
- Very basic throttling based on Promises and SetTimeout per IP
- This is very basic scafolding to which one would develop into a proper queue. Of course you can always: npm i *insert favortie semaphore/bottleneck package here* :)

### Added Dependencies
- Trie-search : Fast and does a good job and radix-tree string searches

## Build and Deploy

### Build Docker Image
Create a docker image tagged with latest git commit hash :

```bash
./docker_build.sh
```

Run your latest build locally :
```bash
docker run -p 2345:2345 bus_bud_challenge_backend:latest
```

### Deploy Image to Heroku
You must login to Heroku container register then run command below to Deploy to heroku
```bash
./docker_heroku_push.sh
```
- Note: Edit *"appname="* in script to change app name for deployment

# **  ORIGINAL UNEDITED README BELOW THIS LINE **
        
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
