# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)


## Important Notes

- Deployed at [Wow you really deploy in only just a push](http://salty-island-8779.herokuapp.com/)
- In order for all the prerequisites to install, here the problem being `toobusy`, `g++` on the machine is required.
- TL;DR; - No kitties were harmed in the creation of this program.

## Steps of the program, or why is it so fast?

### Step 0: Data loading

- The data is loaded inside a Data class to keep the rows of the database.
This class can be easily overloaded for any row based database.
- All of this is supervised by the `TSVImporter` class which loads any TSV file, with any columns format.
- After all of the loading is done the `Data` class holds the data in memory and `TSVImporter` calls the done callback.

### Step 1: String process

- All the data is given to a script to load the data reverse indexed.
Here "name" and "alt_name" columns are used.
- A custom `Analyzer` class is used, in the ElasticSearch mentality.
This analyzer is programmable with a `Tokenizer` and multiple `TokenFilters`.
At first `XRegExp` library was used for their unicode tokens but the scanning of all cities took hours.
- Now the `Tokenizer` first transforms the names related to a city into ascii strings and then tokenizes it.
- The first `TokenFilter` is a simple one, it transforms all tokens to lowercase.
- The second `TokenFilter` expands all strings containing ' or - into multiple combinations of them.
That is the reason why searching for `Hastings` yields `Hastings-on-Hudson` in the results (same thing with Hudson).
- The same analyzer is used on all strings coming into the query search as well.

### Step 1: Data storing

- For word search to be fast as possible, a reverse indexing for cities is create based on the names outputed by the analyzer.
Since we want partial words to yield answers, a Trie is used to keep all names.
- Inside the `Trie`, every `Node` consumes a token from the string of the word to reverse index the object.
- Each `Node` instance in the `Trie` contains two buckets:
	- Object bucket containing the list of words that are finished at this token.
	- Cache bucket containing the cities (up to a certain number) down the Nodes connected to this Node.
- The cache bucket contains the highest populated cities in order.

### Step 2: Retrieval

- A Trie acts almost like a HashTable except it is O(log(n)), where n is a very small string.
- Even if somebody would do a query with a huge string, the n is small since ultimately there are no more Nodes down the road it returns empty.
This brings a problem that I will talk later in the "to improve section" (fuzzy search).
- The trie does not take too much space since tokens are reused and it is a reverse index to the data already loaded in memory.

### Step 3: Ranking

- A first rank is computed based on the population size of a city.
- I thought the distribution would follow a normal distribution, it actually follows an exponential one instead.
- The rank is computed based on that distribution where the cities with over a couple million population size gets about 0.9 and the others 0.02.
These numbers might change as I play with the ranking and change constants.
- The second rank comes from geolocation which is only present when latitude and longitude are given.
Of course this is relative to the cities returned by the trie, therefore a if searching for a name it cannot output another just because it is close.
It follows the same exponential distribution for ranking. The Haversine formula is used with a special check if positions are too close so that Haversine does not return `NaN`.
- The last rank comes from the number of duplicate objects and either they come from the Trie object pool or from the cache pool.


## Things Left

- Since I spent most of the time coding the logic and unit tests (which are quite the few), I didn't have time to do a maintenance revision of the code.
That is why there are still commented out `console.log` and some non-insightful comments.

## How to improve this

- This program only finds results for exact string searches (even though a string can expand).
To implement "fuzzy search" a BK-Tree structure could be used. All strings could have a Levenshtein automata precomputed so that it the Levenshtein algoritm is not O(mn) anymore.
Also, I have read a cool article about how ElasticSearch uses an FSA approach to this.
- My implementation only uses geolocation for relative ranking. A geohash tree could be used (although I did not research much into it).
With that a list of closest cities could be retrived and use them in the ranking.


# Initial README section

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
