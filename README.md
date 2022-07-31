# Busbud Coding Challenge

## Implementation

API endpoint that provides autocomplete suggestions for large cities.
Suggestions are restricted to cities in the USA and Canada with a population above 5000 people.

- the endpoint is exposed at `/suggestions`
- the partial (or complete) search term is passed as a query string parameter `q`
- the caller's location can optionally be supplied via query string parameters `latitude` and `longitude` to help improve relative scores
- the endpoint returns a JSON response with an array of scored suggested matches
  - the suggestions are sorted by descending score
  - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
  - each suggestion has a name which can be used to disambiguate between similarly named locations
  - each suggestion has a latitude and longitude
- the final application is [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).

#### Sample responses

These responses are meant to provide guidance.

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

- All code is written Typescript
- Simple caching of past responses was implemented to handle high levels of traffic.
- Challenge is submitted as pull request against ([original repo](https://github.com/busbud/coding-challenge-backend-c).

## Dataset

You can find the necessary dataset along with its description and documentation in the [`data`](data/) directory.

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
npm run test
```

### Building the application

To compile the application using the TypeScript compiler:

```
npm run build
```

### Starting the application

To compile the application and start a local server run:

```
npm run start
```

it should produce an output similar to:

```
Server running at http://127.0.0.1:2345/suggestions
```
