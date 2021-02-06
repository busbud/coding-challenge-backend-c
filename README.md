# Busbud Coding Challenge

This project contains an API endpoint that provides autocomplete suggestions for large cities. The suggestions are restricted to cities in the USA and Canada with a population above 5000 people (no cities in the data file have over 5000 people).

- the endpoint is exposed at `/suggestions`
- the partial (or complete) search term is passed as a query string parameter `q`
- the caller's location can optionally be supplied via query string parameters `latitude` and `longitude` to help improve relative scores
- the endpoint returns a JSON response with an array of scored suggested matches
    - the suggestions are sorted by descending score
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    - each suggestion has a name which can be used to disambiguate between similarly named locations
    - each suggestion has a latitude and longitude
- the final application is [deployed in Heroku](https://pure-sands-75942.herokuapp.com/suggestions?q=Londo).

## Sample responses

These responses are meant to provide guidance. The exact values can vary based on the data source and scoring algorithm.

**Near match**

    GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

```json

  "suggestions": [
    {
      "name": "New London, WI, USA",
      "latitude": "44.39276",
      "longitude": "-88.73983",
      "score": 1
    },
    {
      "name": "London, 08, Canada",
      "latitude": "42.98339",
      "longitude": "-81.23304",
      "score": 1
    },
    {
      "name": "Londonderry, NH, USA",
      "latitude": "42.86509",
      "longitude": "-71.37395",
      "score": 1
    },
    {
      "name": "New London, CT, USA",
      "latitude": "41.35565",
      "longitude": "-72.09952",
      "score": 1
    },
    {
      "name": "Thunder Bay, 08, Canada",
      "latitude": "48.38202",
      "longitude": "-89.25018",
      "score": 1
    },
    {
      "name": "Gatineau, 10, Canada",
      "latitude": "45.47723",
      "longitude": "-75.70164",
      "score": 1
    },
    {
      "name": "Richmond, IN, USA",
      "latitude": "39.82894",
      "longitude": "-84.89024",
      "score": 1
    },
    {
      "name": "Landover, MD, USA",
      "latitude": "38.934",
      "longitude": "-76.89664",
      "score": 1
    },
    {
      "name": "Cleveland, OH, USA",
      "latitude": "41.4995",
      "longitude": "-81.69541",
      "score": 1
    },
    {
      "name": "Dalton, GA, USA",
      "latitude": "34.7698",
      "longitude": "-84.97022",
      "score": 0.98
    },
    {
      "name": "Edmond, OK, USA",
      "latitude": "35.65283",
      "longitude": "-97.4781",
      "score": 0.91
    },
    {
      "name": "Redmond, OR, USA",
      "latitude": "44.27262",
      "longitude": "-121.17392",
      "score": 0.86
    },
    {
      "name": "Redondo Beach, CA, USA",
      "latitude": "33.84918",
      "longitude": "-118.38841",
      "score": 0.86
    },
    {
      "name": "Richmond, 02, Canada",
      "latitude": "49.17003",
      "longitude": "-123.13683",
      "score": 0.86
    },
    {
      "name": "Coronado, CA, USA",
      "latitude": "32.68589",
      "longitude": "-117.18309",
      "score": 0.86
    },
    {
      "name": "Redmond, WA, USA",
      "latitude": "47.67399",
      "longitude": "-122.12151",
      "score": 0.86
    },
    {
      "name": "Richmond, CA, USA",
      "latitude": "37.93576",
      "longitude": "-122.34775",
      "score": 0.86
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

## Considerations

- All code is written in Javascript.
- Mitigations to handle high levels of traffic should be implemented.

## Dataset

You can find the necessary dataset along with its description and documentation in the [`data`](data/) directory.

## Getting Started

### Prerequisites

You are going to need:

- `Git`
- `nvm` (or your preferred node version manager)
- `Node.js`

### Setting up your environment

1. Begin by cloning this repo.

2. Install [nvm](https://github.com/nvm-sh/nvm#install--update-script) or your preferred node version manager.

3. Install [Node.js](http://www.nodejs.org).

### Setting up the project

In the project directory run:

```
nvm use
npm install
```

### Running the tests

The test suite can be run with:

```
npm test
```

### Starting the application

To start a local server run:

```
npm start
```

it should produce an output similar to:

```
debug: Listening on port 8080
```

# Doubts

Which fields from should be used in the fuzzy search? All of them? I'm assuming only the name, ascii and alt_name should be used.
- The name has strange characters
- The ASCII does get if the search has strange characters
- using the alt_name has performance issues and match most of the cities that have huge alt_names
Do you have a default function to decide on score?
- For the fuzzy search on the name I've looked into the "fuzzy" and "fuzzball" packages
  - fuzzy returns a small list with scores that it thinks are the best
  - fuzzball returns a score for the full list. Not deciding on a
- Right now, only the fuzz can cut results. If the score goes below the treshold when taking into consideration the location, we just reduce the score, we do not remove the entry
Is there a limit to the array size? If not, is there a safe score where I can say that an entry shouldn't be returned?
Could I add caching up to what point?
Should I scape wildcards?
What should be the logging format?
Do I need to set rate limitting on the endpoint? If yes, by IP?
Can just latitude or longitude be passed?
Do I need to use a database for it? Since the data is immutable and less than 1Mb I thought it was alright to just load it in memory

# Tools

- Using [ESLint](https://eslint.org/) for code style. Install the eslint plugin for your desired IDE.
- Using [go-wrk](https://github.com/tsliwowicz/go-wrk) for easy peformance testing.


# Why MVC

The [nodebestpractices](https://github.com/goldbergyoni/nodebestpractices#1-project-structure-practices) talks about organizing the repo by components. My experience so far is that breaking the repo into components is bad for microservices. Everything is good until you have components that do not belong to a single component. You may also end up with more files in a single folder than when using the usual MVC separation, since microservices usually endup having a single component.