# Busbud Coding Challenge - Weslley Alcobaça

## Implementation

This is an API endpoint that provides autocomplete suggestions for large cities.
The suggestions are restricted to cities in the USA and Canada with a population above 5000 people.
This API was designed as part of Busbud's hiring process.

A live version of the API endpoint can be found at [Heroko](https://busbud-cod-chal-weslley.herokuapp.com/suggestions)

### Query Parameters

- `q` - the query used for suggestions
- `latitude` - caller's latitude, used to improve relative scores
- `longitude` - caller's longitude, used to improve relative scores
- `max_amount` - the maximun number of suggestions to be returned
- `loose_match` - if "true", will attempt to match suggestions, even if there are small typos. However, the option reduces performance and score's accuracy

### Sample queries

A few examples of requests made to the API.

**Near match**

    GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163&max_amount=3&loose_match=false

```json
{
  "suggestions": [
    {
      "name": "London, 08, Canada",
      "latitude": "42.98339",
      "longitude": "-81.23304",
      "score": 0.8
    },
    {
      "name": "London, OH, USA",
      "latitude": "39.88645",
      "longitude": "-83.44825",
      "score": 0.5
    },
    {
      "name": "Londontowne, MD, USA",
      "latitude": "38.93345",
      "longitude": "-76.54941",
      "score": 0.4
    }
  ]
}
```

**Loose match**

    GET /suggestions?q=Lodon&max_amount=3&loose_match=true

```json
{
  "suggestions": [
    {
      "name": "London, 08, Canada",
      "latitude": "42.98339",
      "longitude": "-81.23304",
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
      "score": 0.5
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

- All code was written in Javascript.
- In order to mitigate high levels of traffic, a simple memory cache was implemented. The implementation is naive and can bring many memory problems in a production environment. It is supposed to be used as a proof of concept only. In a production environment, more robust solutions, like Redis make much more sense.
- No extra dependencies were directly added, however, node and mocha's version were updated.

## Getting Started

### Prerequisites

You are going to need:

- `Git`
- `nvm` (or your preferred node version manager)
- `Node.js` - 13.14.0 or higher

### Setting up your environment

1. Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and
[Windows](http://windows.github.com/) that make this easier.

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
npm run start
```

it should produce an output similar to:

```
Server running at http://127.0.0.1:2345/suggestions
```
