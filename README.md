
# Busbud Coding Challenge 

## Requirements

Design an API endpoint that provides auto-complete suggestions for large cities.
The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

- the endpoint is exposed at `/suggestions` ✅
- the partial (or complete) search term is passed as a querystring parameter `q` ✅
- the caller's location can optionally be supplied via querystring parameters `latitude` and `longitude` to help improve relative scores ✅
- the endpoint returns a JSON response with an array of scored suggested matches ✅
    - the suggestions are sorted by descending score
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    - each suggestion has a name which can be used to disambiguate between similarly named locations
    - each suggestion has a latitude and longitude
- all functional tests should pass (additional tests may be implemented as necessary). ✅
- the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).
- feel free to add more features if you like! ✅

- searching across ~125000 cities (db contains cities with population > 1000) ✅
- support misspelling ✅

### Non-functional

- All code should be written in Javascript
- Mitigations to handle high levels of traffic should be implemented
- Challenge is submitted as pull request against this repo ([fork it](https://help.github.com/articles/fork-a-repo/) and [create a pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)).
- Documentation and maintainability is a plus

### References

- Geonames city and other lists http://download.geonames.org/export/dump



## Getting Started

Clone the project

### Setting up the project

In the project directory run

```
npm install
```

### Configure

Just copy config.example to config.json and fill it

```
cp config.example config.json
```

### Starting the application

To start a local server run

```
npm start
```

### Starting the application as daemon

Need to install [forever](https://www.npmjs.com/package/forever) if not exist

```
npm install forever -g
```

Then run (if need to restart use again)

```
sh app.sh
```

### Running the tests

The test suite can be run with

```
npm test
```



## How to use 

### GET /suggestions

**params:**

```
  q - string
  latitude - [-90, 90] (optional)
  longitude - [-180, 180] (optional)
  population - number (by default > 5000)
```

**examples**

with q only

```
  /suggestions?q=lon
```

with q and LatLng (Moscow)

```
  /suggestions?q=lon&latitude=55.753842&longitude=37.621166
```

with q and LatLng (Montreal)

```
  /suggestions?q=london&latitude=45.5022&longitude=-73.6063
```


