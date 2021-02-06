# Busbud Coding Challenge

This project contains an API endpoint that provides autocomplete suggestions for large cities. The suggestions are restricted to cities in the USA and Canada with a population above 5000 people (no city in the data file has over 5000 people).

- the endpoint is exposed at `/suggestions`
- the partial (or complete) search term is passed as a query string parameter `q`
- the caller's location can optionally be supplied via query string parameters `latitude` and `longitude` to help improve relative scores
- the endpoint returns a JSON response with an array of scored suggested matches
    - the suggestions are sorted by descending score
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    - each suggestion has a name which can be used to disambiguate between similarly named locations
    - each suggestion has a latitude and longitude
    - I've added a limit of 20 results that could be changed to be a query parameter as well
- the final application is [deployed in Heroku](https://pure-sands-75942.herokuapp.com/suggestions?q=Londo).

## Sample responses

These responses are meant to provide guidance. The exact values can vary based on the query and location.

**Near match**

    GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

```json
{
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

or optionally with coverage:

```
npm run test:coverage
```

### Starting the application

To start a local server run:

```
npm start
```

it should produce an output similar to:

```
debug: Listening on port 8080
debug: Listening on port 8081
```

The application is started on the first port (in this case 8080) while we start another server at 8081 for health and metrics endpoints. There is also a swagger-ui configured together with the application at `/api-docs`.

# Evaluation
## Doubts and Considerations

- Which fields from the data should be used in the fuzzy search? I'm assuming only the name, ascii and alt_name should be used, all together.
  - The name has strange characters;
  - The ASCII does match if the name has strange characters;
  - Using the alt_name has performance issues and match most of the cities that have huge alt_names if the cutoff is not high enough (over 50).

- Do you have a default function to decide on score?
  - For the fuzzy search I've looked into the "fuzzy" and "fuzzball" npm packages. The "fuzzy" package had better performance, but I went with "fuzzball" because it had the functionalities I was looking for.

- Right now, only the fuzz search can cut results from the final list. We could return more results from the repository and limit the result after the location was taken into consideration if that is an issue.

- Is there a limit to the array size? If not, is there a safe score where I can say that an entry shouldn't be returned even after taking into consideration the location? For now I just limited it to 20 entries. The use case probably influences this decision a lot.

- Could I add caching up to what point? The node-cache package doesn't have a max size (only max entries), so it may incur in memory overflow. I usually only add in-memory caches if I have an idea of how big the cache is going to be.

- Should I scape wildcards and other strange characters from the query?

- Is there a logging standard that I should follow?

- Do I need to set rate limitting on the endpoint? If yes, by IP? I don't know much about heroku to set that up, but I'd usually do it using an API gateway.

- Can just latitude or longitude be passed (one of them but not both)? What should I do in this case?
  - Should I just ignore it?
  - Use just that value to take the location into consideration?
  - Return an error? Right now that is what I'm doing (I'm returning an error if just one of them is passed).

- Did I need to use a database for it? Since the data is immutable and less than 1Mb I thought it was alright to just load it in memory, but postgresql has a fuzzy search if we need to do so.
  
- How do you feel about adding a minimum length for the query? Like 3 characters? It's what is usually done when there are too many options.

- There are still some testes missing for the /src/service/suggestionService.js

- Do I know more about the use case? If I know that it will be used in a textbox to show suggestions, there are assumptions that could be used to improve the performance:
  - In textboxes, people usually write continuously. Assuming that adding letters will only decrease the score for a given entry, we can improve caching by checking if there is already a smaller version of the string cached and using the list from that as a new list for the fuzzy search. A smaller list will significantly improve the endpoint response. E.g.: an user is looking for "London". First he types 'L', and we cache the resulting list. Then when he adds 'o', which then triggers a fuzzy search on the 'L' cached list. This may also be better cached as a search tree/graph.

There are more considerations in TODOs and FIXMEs in the code.

## Tools

- Using [ESLint](https://eslint.org/) for code style. Install the eslint plugin for your desired IDE.
- Using [go-wrk](https://github.com/tsliwowicz/go-wrk) for easy local peformance testing. After installing, just run:
```console
  go-wrk -d 5 http://localhost:8080/suggestions\?q\=lond
```
it isn't really a fair test because of the in-memory cache.

## Why MVC vs resource/component folder separation

The [nodebestpractices](https://github.com/goldbergyoni/nodebestpractices#1-project-structure-practices) talks about organizing the repo by components. My experience so far is that breaking the repo into components is bad for microservices. Everything is good until you have components that do not belong to a single component. You may also end up with more files in a single folder than when using the usual MVC separation, since microservices usually endup having just a few components.