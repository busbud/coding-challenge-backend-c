# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Demo

[/suggestions?q=Montre](https://city-suggestions.herokuapp.com/suggestions?q=Montre)

[/suggestions?q=Montre&longitude=-74.58438&latitude=46.21274&radius=100](https://city-suggestions.herokuapp.com/suggestions?q=Montre&longitude=-74.58438&latitude=46.21274&radius=100)


Using httpie:

```
http https://city-suggestions.herokuapp.com/suggestions q=='San Fr' latitude==37.77493 longitude==-122.41942 radius==100
```

should return
```
HTTP/1.1 200 OK
Connection: keep-alive
Content-Length: 326
Content-Type: application/json; charset=utf-8
Date: Fri, 29 Jun 2018 01:55:08 GMT
Etag: W/"146-XcEW9GNeMiO0Cw+rL0HchXBLcaE"
Server: Cowboy
Via: 1.1 vegur
X-Ratelimit-Limit: 120
X-Ratelimit-Remaining: 119

{
    "suggestions": [
        {
            "latitude": 37.77493,
            "longitude": -122.41942,
            "name": "San Francisco, CA, US",
            "score": 0.9203846153846154
        },
        {
            "latitude": 37.65466,
            "longitude": -122.40775,
            "name": "South San Francisco, CA, US",
            "score": 0.789357894736842
        },
        {
            "latitude": 37.42411,
            "longitude": -122.16608,
            "name": "Stanford, CA, US",
            "score": 0.5291374999999999
        }
    ]
}
```

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

    GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163&radius=100

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

#### production

To start a local server run: `npm start`

which should produce output similar to

```
Server running at http://127.0.0.1:2525/suggestions
```

#### development

To automatically restart the server when you save changes run: `npm start:dev`


### Deployment

#### Heroku

Make sure you are login with your account: `heroku login`

Create the app: `heroku create`

Push your master branch: `git push heroku master` or a another branch: `git push heroku <branch>:master`

Then you'll be able to access it (httpie): `http http://heroku-app-subdomain.herokuapp.com/suggestions q=='San F' latitude==37.77493 longitude==-122.41942 radius==100`


### Continuous Integration

We use circleci to build and test each branch before they are merged to master, and then deploy master to heroku when merged to master.

Make sure you have setup your app with Heroku first.

Then add the following environment variables into your circleci project:
```
HEROKU_APP_NAME=app_name
HEROKU_API_KEY=$(heroku auth:token)
```


## Improvements

#### CityRepository

Since the set is small, I'm loading the list of cities in memory in order to filter on it when doing the search.

Performance could be improved if we were using a database: elasticsearch seems like a good candidate here (location + text search), but others could do the job as well, and for a small set of data like this, it wouldn't really matter.)

#### General Project Structure

My last professional experience with Node was 3-4 years ago. I'm a bit rusted when it comes to the right project structure.

I tried to follow a structure you could find in a [DDD](https://en.wikipedia.org/wiki/Domain-driven_design) project:

```
presentation
application
domain
infrastructure
```

But since the `domain` would be really simple here (City), I haven't created one. Also, the `application` services are mostly orchestrators between the domain model and the infrastructure, used by the `presentation` layer. In our case, the `presentation` is directly calling the `infrastructure`, since it's a "simple" application, it doesn't really make sense to add that complexity here.


#### Scoring

I've spent some time looking for some good scoring algorithms/libraries but ended up with some basic ones. I'm pretty sure we can use better scoring algorithm for the name matching. The distance scoring is pretty dumb too.

Also, I've made the arbitrary decision to weight the distance higher than the name score. The formula I came up with is `0.7 * S(distance) + 0.3 * S(name)` :shrug:

#### Limit

I've added an arbitrary limit of the first 20 best suggestions. We could add a parameter to handle pagination/offset of the suggestions we return.