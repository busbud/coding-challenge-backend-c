# Busbud Coding Challenge Submission

## Suggestions link
[http://filter-cities.herokuapp.com](http://filter-cities.herokuapp.com)

## Approach

Given the `.tsv` data set, the first thing to do was to convert the data to JSON.
This was done using the `event-stream` package as well as the `stream-reduce` package.

When the `Express` server is started, if the converted JSON file does not exist, the `.tsv`
file is converted to a JSON file and saved in the `data` directory. The code responsible for this is in the `/app/tsv2json.js` file.
Once the JSON file is created, a data store is initialized with the JSON data. The store is then seeded
with data based on the frequency of the first letter of the city's ASCII name. In other words, city's starting
with the most common first letters in the data set are added to the store.

The `/app/store.js` file contains the storing and caching functionality. When a search query is received, if the
query term does not exist in the cache, then the seeded store is checked. If the data is not there, then the store
will search in the original data set and cache the results for future use.

When a request is made to the `/suggestions` route, a query object containing the search term, longitude and latitude, is passed
to the store. The store then retrieves the data, caches the data using the query term as key and finally computes the suggestion
score.

## Score Computation

The score is calculated using the following simple metric:

### GeoScore

calculated by taking the ratio of the city's own geo information to the default geo information (Montreal) and computing the average

### TermScore

calculated using the length of the search query versus the city's name length. if the search query's length is greater than the 
name of the city, the score is dropped 0.1 to indicate a loss of relevance in the result

### PopulationScore    

ratio of the city's population to the city in the result set with the highest population. Score is biased to city with higher population in
case cities share the same name.

The final score is the product of the three scores above. There is a partiality towards the geoScore. If the geoScore is greater than 0.5, then
that value is used as the final score

## Installation

Once the repository is cloned, run `npm install` to install the dependencies.

The server can be started by running the following command `npm run start`

Once the server is started, a new file `cities_canada-usa.json` should be created in the data directory, if it does not already exist.

To run tests, `npm run test`
