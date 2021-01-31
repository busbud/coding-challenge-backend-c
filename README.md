# Busbud Coding Challenge

## Requirements

Design an API endpoint that provides autocomplete suggestions for large cities.
The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

- the endpoint is exposed at `/suggestions`
- the partial (or complete) search term is passed as a query string parameter `q`
- the caller's location can optionally be supplied via query string parameters `latitude` and `longitude` to help improve relative scores
- the endpoint returns a JSON response with an array of scored suggested matches
    - the suggestions are sorted by descending score
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    - each suggestion has a name which can be used to disambiguate between similarly named locations
    - each suggestion has a latitude and longitude
- all functional tests should pass (additional tests may be implemented as necessary).
- the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).
- feel free to add more features if you like!

#### Sample responses

These responses are meant to provide guidance. The exact values can vary based on the data source and scoring algorithm.

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

- All code should be written in Javascript, Typescript or PHP.
- Mitigations to handle high levels of traffic should be implemented.
- Challenge is submitted as pull request against this repo ([fork it](https://help.github.com/articles/fork-a-repo/) and [create a pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)).
- Documentation and maintainability is a plus.

## Dataset

You can find the necessary dataset along with its description and documentation in the [`data`](data/) directory.

## Evaluation

We will use the following criteria to evaluate your solution:

- Capacity to follow instructions
- Developer Experience (how easy it is to run your solution locally, how clear your documentation is, etc)
- Solution correctness
- Performance
- Tests (quality and coverage)
- Code style and cleanliness
- Attention to detail
- Ability to make sensible assumptions

It is ok to ask us questions!

We know that the time for this project is limited and it is hard to create a "perfect" solution, so we will consider that along with your experience when evaluating the submission.

## Getting Started

### Prerequisites

You are going to need:

- `Git`
- `nvm` (or your preferred node version manager)
- `Node.js`

### Setting up your environment

1. Begin by forking this repo.

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
{"message":"Listening on port 8080","level":"debug"}
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