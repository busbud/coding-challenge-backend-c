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
Server running at http://127.0.0.1:2345/suggestions
```

## Implementation

**Heroku Deployment:** https://busbud-autcomplete-challenge.herokuapp.com/

### Approach

**Loading the Data**

When the server is launched, 'updateRecords()' is called, which populates several arrays with the information needed from the tsv file. The 'cityIDs' array holds different possible city names users could search for (ie. New York City would consist of New York City, York City, and City to allow queries starting at any of the words) along with an id for that city. The id is then used to look up the city's full name, longitude, latitude, and population in the arrays 'cities', 'longs', 'lats', and 'populations' respectively. The 'cityIDs' array is also sorted after it is made to allow a 'O(log(N))' lookup time. 

**Querying the Data**

When a city is searched for using a prefix, the input parameters are first checked to be valid. There are 4 potential parameters:

    1. q: String
    2. longitude: float
    3. latitude: float
    4. limit: int

'q' is the only mandatory parameter while the other three are optional. When a query is submited, the 'cityIDs'is searched, in a binary fashion for 'O(log(N))' time, and and the prefix is compared to the substring of the array inputs. When a match is found, the index corresponding to the first substring-prefix match in the 'cityIDs' array is then produced. From there, the 'cityIDs' array is then iterated until a city name substring does not match the prefix and every ID found is used to add the full city name (city, state/province, country), longitude, latitude, and score into an array. The array is then sorted by score and lexicographically before it is returned.

**Scoring**

I chose to implement two ways of scoring cities. The first is when no user coordinates are provided. The score is calculated based on the population of the city. Cities with greater than 90 000 people receive a score of 1.0 and the rest are calculated by the formula '(population + 10 000)/100 000'.

The second method of scoring is done when user coordinates are provided. It has two categories; population and distance. Population account for 0.3 of the score and distance accounts for 0.7. I calculated the distance between the city and the user using the haversine formula. If the city is less than 20 km away it receives a distance score of 1.0 and if the city is more than 2000 km away it receives a score of 0. All other cities are given a score based on the formula '1-(distance/2000)'. The population score for this method is calculated the same way as above.

**Front End**

I implemented a basic front end to show how the autocomplete could work for users. Hope you enjoy!

**Other Implementations**

I considered iterating through all city names searching for any substring in the name that matches the prefix (ie. New York City is returned by ork) but that would require iterating through all 'N' city names which, for large datasets, could take quite awhile. To solve this I considered storing all suffixes to each city name (ie. New York City is stored as New York City, ew York City, w York City, York City, etc.) but ultimately decided this would cause a large amount ('O(log(N*M))' where M is the average city name length) of memory needed.

The scoring algorithm could also be tweaked depending on preferences (ie. changed weighting or other categories).
