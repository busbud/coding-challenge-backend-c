# Busbud Coding Challenge city suggestion API
[![Code Climate](https://codeclimate.com/github/stealth-coaxial/coding-challenge-backend-c/badges/gpa.svg)](https://codeclimate.com/github/stealth-coaxial/coding-challenge-backend-c) [![Build Status](https://travis-ci.org/stealth-coaxial/coding-challenge-backend-c.svg?branch=master)](https://travis-ci.org/stealth-coaxial/coding-challenge-backend-c)
## Overview
The app is live here: https://busbud-suggestions.herokuapp.com/suggestions


### Framework and database choices
The API runs in an expressJS server and the cities data live in MongoDB.

I picked expressJS because it is a widely used framework, it has an extensive documentation and chances are that other web services at Busbud also use express.

For the cities, I initially went with ElasticSearch because it is very scalable and the cities are not likely to be updated regularly. With the right settings for the index, ElasticSearch is a very powerful engine that could nicely back the API.

However, setting up ElasticSearch was a larger task than I anticipated and after spending 2 days on it, I decided to go another route and use MongoDB instead.

MongoDB is very well documented, uses JSON documents like ElasticSearch but is much easier to setup and readily available on Heroku's free tier. It can also be scaled up, should need be.

The app is live here: https://busbud-suggestions.herokuapp.com/suggestions
### About the code
I am a beginner with NodeJS and this was a fun opportunity to learn more about the platform. Async programming is a very interesting approach to network operations and I want to learn how to use it better and better.

I meant to split the various components into modules so it doesn't clutter the expressJS app but I couldn't get it done because of scoping problems. The objects `res`, `req` and `app` couldn't be accessed from the modules and I had to abandon the idea because time was running out.

As a middle-ground, I divided the various parts of the app into named functions to help with readability and to avoid callback hell.

### Things that could be improved
- The scoring algorithm could be improved by using dynamic maximum values instead of absolute ones. I think iterating over every suggestion, finding what the largest population count is and using this as the denominator would give better relative scores. Same thing goes for the distance and exactitude. I didn't implement it because I was running out of time and the current scoring algorithm gave sane results.
- The code could be refactored and split into modules, maybe simplified too. This is one of the major areas I would like to improve myself on: spotting where and how to refactor further.
- Reduce the amount of data the app is searching through by discarding the columns we don't need (feat_class, feat_code, cc2, admin2, admin3...)
- I'm pretty sure it is possible to achieve this app without resorting to expressJS and using the http module only. expressJS provided an easier to use framework but ultimately, http would be more barebones.
- MongoDB might not be the absolute best choice for speed and Redis might be an even better alternative to ElasticSearch. I chose MongoDB knowing it wasn't as suited as Redis because I didn't want to run out of time figuring it out while I could get a working API with MongoDB. If I had more time, I would have definitely looked into Redis.

## Reference
The API has only got one endpoint.
### Endpoint `/suggestions`
When supplied a partial or full city name, it will return a list of suggestions matching the search term. Note that it will also search into alternative names for a city so it is possible to find Los Angeles with the query "LAX" or Toronto with the query "YTO".


To receive suggestions, `GET /suggestions` with the following parameters:

Name | Type | Default value | Description
--- | --- | :---: | ---
`q` | String | undefined | The partial or complete search term (mandatory)
`latitude` | Number (float) | undefined | The caller's latitude to improve relative scores (optional*)
`longitude` | Number (float) | undefined | The caller's longitude to improve relative scores (optional*)
`limit` | Number (integer) | 8 | Limits the number of results (optional)
*Note that you should supply both or none at all.

The returned data will be a JSON object containing an array of suggestions with the following properties:

Property | Value
--- | ---
`name` | The ASCII name of the city with the province and the country
`latitude` | The city's latitude
`longitude` | The city's longitude
`score` | The confidence in the suggestion (See [suggestions scoring](#suggestions-scoring) for more details)

- A successful search (i.e. a search that yielded at least one result) will return with a HTTP status code 200.  
- A search without a `q` parameter will return a 400 status code and an error.  
- A search yielding no results will return an empty array with a 404 status code.

The suggestions are ordered by score, from the highest to the lowest.

#### Sample queries and answers
`GET /suggestions?q=nyc` returns:
```json
{
    "suggestions":
    [
        {
            "name": "New York City, NY, USA",
            "latitude": 40.71427,
            "longitude": -74.00597,
            "score": "0.4615"
        }
    ]
}
```

`GET /suggestions?q=mont&latitude=45.56995&longitude=-73.692&limit=7` returns:
```json
{
    "suggestions": [
        {
            "name": "Montreal, QC, Canada",
            "latitude": 45.50884,
            "longitude": -73.58781,
            "score": "0.9000"
        },
        {
            "name": "Mont-Royal, QC, Canada",
            "latitude": 45.51675,
            "longitude": -73.64918,
            "score": "0.7819"
        },
        {
            "name": "Deux-Montagnes, QC, Canada",
            "latitude": 45.53455,
            "longitude": -73.90168,
            "score": "0.7589"
        },
        {
            "name": "Montreal-Ouest, QC, Canada",
            "latitude": 45.45286,
            "longitude": -73.64918,
            "score": "0.7577"
        },
        {
            "name": "Mont-Saint-Hilaire, QC, Canada",
            "latitude": 45.56678,
            "longitude": -73.19915,
            "score": "0.7460"
        },
        {
            "name": "Saint-Bruno-de-Montarville, QC, Canada",
            "latitude": 45.53341,
            "longitude": -73.34916,
            "score": "0.7332"
        },
        {
            "name": "Bromont, QC, Canada",
            "latitude": 45.31678,
            "longitude": -72.64912,
            "score": "0.6049"
        },
    ]
}
```

### Suggestions scoring
Cities are scored based on the confidence this is the result the user was after and depends on 3 criterions:
- The city's size by population _(larger means a higher score)_
- How much the city's name matches the search query (_closer means a higher score_)
- How close it is to the user (_closer means a higher score_)


The weighted average of these criterion is the final suggestion score.

#### Population
The `POPULATION_THRESHOLD` is set to 1,000,000 habitants. A city with a million habitants or more will score full points on this criteria and a city with less will score proportionally.  
1,000,000 habitants  is an arbitrary value but it seemed to be reasonable enough to assume it makes the city a major one where a significant number of people want to travel to/from.

#### Query exactitude
The length of the query is compared to the length of the city's ASCII name. The resulting ratio gives the score for the exactitude criteria.  
**Caveat**: this works well when searching for the city's name but gives an anormally low score when searching for the alternative name. For example, searching for "NYC" returns only one result but the score is low when it should be very close to 1. This is because it compares the length of "NYC" (3) to the length of "New York City" (13). 3/13 ≈ 0.23

#### Distance
This score is only calculated if `latitude` and `longitude` were supplied in the query.
This measures how far away the city is, on the assumption that users are more likely to search for  cities closer to them rather than further ones. The suggestion gets full points when it is less than 50 km away. It gets less points the further away it is, dropping to 0 points if it is further than 1,000 km away.

## Testing

The tests are located in `test/suggestions.js` and can be run with `npm test`.

I made a few additions to the test suite and fixed two tests that seemed to have mistakes in them.

## Resources that helped me tremendously
- The good people of #node.js on freenode
- callbackhell.com
- https://github.com/maxogden/art-of-node
- http://msdn.microsoft.com/en-us/magazine/gg309172.aspx for documenting the API
- Wikipedia for the Haversine formula's uses
