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

#done - Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and
[Windows](http://windows.github.com/) that make this easier.

### Setting up a Nodejs environment

#done - Get started by installing [nodejs](http://www.nodejs.org).

#done - For OS X users, use [Homebrew](http://brew.sh) and `brew install nvm`

Once that's done, from the project directory, run

```
#done  - nvm use
```

### Setting up the project

In the project directory run

```
#done - npm install
```

### Running the tests

The test suite can be run with

```
# tests - npm test
```

### Starting the application

To start a local server run

```
PORT=3456 npm start
```

which should produce output similar to

```
#done - Server running at http://127.0.0.1:2345/suggestions
```

### EXTRA BY CARLOS

Node.js was installed at

   /usr/local/bin/node

npm was installed at

   /usr/local/bin/npm

Make sure that /usr/local/bin is in your $PATH.

==> Caveats
Add NVM's working directory to your $HOME path (if it doesn't exist):

  mkdir ~/.nvm

Copy nvm-exec to NVM's working directory

  cp $(brew --prefix nvm)/nvm-exec ~/.nvm/

Add the following to $HOME/.bashrc, $HOME/.zshrc, or your shell's
equivalent configuration file:

  export NVM_DIR=~/.nvm
  source $(brew --prefix nvm)/nvm.sh

Type `nvm help` for further information.

### TO ADD IN DOCUMENTATION BY CARLOS

how to set bash command in OS X: http://superuser.com/questions/147043/where-to-find-the-bashrc-file-on-mac-os-x-snow-leopard-and-lion
it's good to build APIs based on express (reference needed)

### HINTS BY CARLOS
from Nicholas Chaulet: 
API endpoint is disponible at https://busbud-coding-chalenge.herokuapp.com/suggestions?q=montreal and UI at https://busbud-coding-chalenge.herokuapp.com

http://gruntjs.com/
http://expressjs.com/
    - to provide structure to my api
    - to ease routing in the api
    - to ease parsing of queries
https://babeljs.io/

use postman to test the GETs

explain how you used tsv-to-json and then sent it to algolia for indexing

good explanation of module.exports: http://www.sitepoint.com/understanding-module-exports-exports-node-js/

the algolia search helper returns a JavaScript objet in this format: https://github.com/algolia/algoliasearch-helper-js#results-format
http://stackoverflow.com/questions/26484394/algolia-vs-solr-search
http://www.quora.com/How-does-Elasticsearch-relate-and-or-compare-to-Algolias-Search-as-a-Service

http://www.geonames.org/US/administrative-division-united-states.html
http://www.geonames.org/CA/administrative-division-canada.html

the geolocation came from: http://www.movable-type.co.uk/scripts/latlong.html

algolia for ui; https://github.com/algolia/algoliasearch-helper-js/blob/master/examples/instantsearch%2Bhelper.html

http://stackoverflow.com/questions/16116768/get-the-latitude-and-longitude-using-javascript

http://127.0.0.1:3000/suggestions?q=Montr&latitude=45.5151100&longitude=-73.6780340&geo=500

//TODO: remove duplicates by name and state. ex: Monticello,KY appears twice in geonames due to timezone

# FEATURES BY CARLOS
maps.googleapis.com/maps/api/geocode/json