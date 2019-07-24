# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/jubeless/coding-challenge-backend-c/tree/master.png?circle-token=c3a1acdb7897118e0c8445b53cc4c35a0fa26e07)](https://circleci.com/gh/jubeless/coding-challenge-backend-c/tree/master)
## Submission

- Uses [expressjs](https://github.com/expressjs/express) as a server
- We use redis to implement caching. caching configuration can be found in `config.caching`
- Added busbud-lint

#### Importing Data
City data is imported in the `importData.js` file. We utilize Node Stream to pipe individual lines of the file and process each one individual. Currently we are simply storing the cities in-memory. We can easily add a pipe to store the cities in a Database.   

#### Routes
###### Suggestions
- `[GET] /suggestions`: Implements the standard suggestions endpoint without any streaming as per the minimum requirements
    
###### Stream
- `[GET] /stream`: Implements the suggestions endpoint with streaming. Streaming directly from the tsv file. As we process the lines we are formatting, searching and outputting the suggestions:
    1. Create a readable stream from tsv file
    2. Pipe into city population formatter
    3. Pipe into city coordinate formatter
    4. Pipe into city state formatter
    5. Pipe into filter by city based on config
    6. Pipe into search by search term
    7. Pipe into add score to city
    8. Pipe into city to json formatter
    9. Pipe into suggetions array

#### Scoring Algorithm

The scoring algorithm is comprised of two parts:
- The `search_term` score
        The `search_term` score is a float between 0 and 1 which represents the confidence level of the match. It is calculated by dividing the length of the `search_term` by the length of the city's name.  
        ```search_term.length / city.name.length```  
        If the lengths match it would be an full match and yield a value of 1. If it is a partial match then the length of `search_term` will be inferior to the length of city's name and will yield a fractional value less then 1.
                
- The `distance` score
        The `distance` score is a float between 0 and 1 which represents the closeness of two coordinates, where 1 implies both coordinate are at the same exact point and 0 mean they are the farthest apart. It is calculated by taking the difference between the biggest possible distance and the distance between both point and dividing that by the biggest possible distance between 2 coordinates on earth.  
        
        EARTH_CIRCUMFERENCE = 40075.0 // in kilometers
        HALF_EARTH_CIRCUMFERENCE = EARTH_CIRCUMFERENCE / 2.0 // biggest ditance between 2 coordinates on earth
        distanceInKm = distanceBetweenCoordinates(coordinate_a, coordinate_b)
        ((HALF_EARTH_CIRCUMFERENCE - distanceInKm) / HALF_EARTH_CIRCUMFERENCE)
                 
Once we retrieve both scores we weight them based on the configuration value `config.suggestionConfig.coordinateScoreWeight`. If we are not taking into account `distance` score (since query does not contain latitude and longitude) we would simply use 1 as the weight score for `search_term` score

```
final_score = (search_term_score * searc_term_score_weight) + (distance_score * distance_score_weight)   
```

#### File structure
```
├── Makefile
├── README.md
├── app.js
├── config.js
├── data
│   ├── README.md
│   ├── admin_1_code.js
│   ├── cities_canada-usa-2.tsv
│   └── cities_canada-usa.tsv
├── domain
│   ├── suggestor.helper.js
│   └── suggestor.js
├── dump.rdb
├── lib
│   ├── configureRedis.js
│   └── importData.js
├── package-lock.json
├── package.json
├── routes
│   ├── index.js
│   ├── routes.helper.js
│   ├── stream.js
│   └── suggestions.js
└── test
    ├── domain
    │   └── suggestor.helper.js
    └── suggestions.js
```

#### CI/CD
The application is deployed on Heroku at https://geosuggest.herokuapp.com and it tested via CircleCI. I setup heroku to auto deploy when the master branch on github pass circle ci's continuous integration.  

#### Todo
- Wrap json response on endpoint `/stream/beta` with a root
- Implement Caching on endpoint `/stream/beta`

#### Resources
-   https://www.freecodecamp.org/news/node-js-streams-everything-you-need-to-know-c9141306be93/
-   https://stackoverflow.com/questions/990904/remove-accents-diacritics-in-a-string-in-javascript
-   
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
