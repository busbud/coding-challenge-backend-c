# Solution

Access my solution at https://city-search-node-api.herokuapp.com/.
The index file contains further instructions as to how to communicate with the API.

## Quickstart:

Registering a user:

``` http
  POST https://city-search-node-api.herokuapp.com/register
  Content-Type: application/json

  {
    "username": "test",
    "password": "test"
  }
```

Login as registered user:

```http
  POST https://city-search-node-api.herokuapp.com/login
  Content-Type: application/json

  {
    "username": "test",
    "password": "test"
  }
```

Initiate a search:

``` http
  GET https://city-search-node-api.herokuapp.com/suggestions?q=Springfield&latitude=30.92&longitude=-83.01
```
NOTE: `latitude` and `longitude` are optional parameters and help to enhance the search result. The result will look something like this:
    
``` http
  HTTP/1.1 200 OK
  Server: Cowboy
  Connection: close
  X-Powered-By: Express
  Content-Type: application/json; charset=utf-8
  Content-Length: 642
  Etag: W/"282-JHMr5fIloro00fO+IembddtCuG4"
  Date: Mon, 31 Aug 2020 16:20:00 GMT
  Via: 1.1 vegur

  {
    "suggestions": [
      {
        "name": "Springfield, FL, US",
        "latitude": "30.15326",
        "longitude": "-85.61132",
        "score": 0.8244593514323569
      },
      {
        "name": "Springfield, TN, US",
        "latitude": "36.50921",
        "longitude": "-86.885",
        "score": 0.5218285550494224
      },
      {
        "name": "Springfield, OH, US",
        "latitude": "39.92423",
        "longitude": "-83.80882",
        "score": 0.3307838530379018
      },
      {
        "name": "Springfield, IL, US",
        "latitude": "39.80172",
        "longitude": "-89.64371",
        "score": 0.22963988476827168
      },
      {
        "name": "Springfield, MO, US",
        "latitude": "37.21533",
        "longitude": "-93.29824",
        "score": 0.21537737974225335
      },
      {
        "name": "Springfield, PA, US",
        "latitude": "39.93067",
        "longitude": "-75.32019",
        "score": 0.18715701620439196
      }
    ]
  }
```
   
Lastly you can also delete your 'account':

``` http
  DELETE https://city-search-node-api.herokuapp.com/deregister
```



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

- All code should be written in Javascript or Typescript.
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

### Starting the application

To start a local server run:

```
npm run start
```

it should produce an output similar to:

```
Server running at http://127.0.0.1:2345/suggestions
```
