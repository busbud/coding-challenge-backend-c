# Busbud Coding Challenge [![Build Status](https://circleci.com/gh/busbud/coding-challenge-backend-c/tree/master.png?circle-token=6e396821f666083bc7af117113bdf3a67523b2fd)](https://circleci.com/gh/busbud/coding-challenge-backend-c)

## Implementation

### Data storage and indexation

Because the city set is relatively small, it would have been inefficient to put it in a database (mongodb for instance). Moreover, it wouldn't have helped me to compute scoring.
So I choosed to read the cities file once, and put it in a tree structure in memory.
Each node of the tree is a javascript object having a level (its depth in the tree) containing an array of cities and an object containing the subnodes. Each subnode beeing accessible by a property that is a letter. 

Graphically we could represent it like that (this is a very small sample):

```json
{
  level:0,
  cities:[],
  subNodes: {
    "c": {
      level: 1,
      cities: ['Chicago', 'Cheyenne'],
      subNodes: {
        "h": {
          level: 2,
          cities: ['Chicago', 'Cheyenne'],
          subNodes: {
            "e": {
              level: 3,
              cities: ['Cheyenne'],
            },
            "i": {
              level: 3,
              cities: ['Chicago'],
              suNodes: { /* and so on */ }
            }
        }
      }
    },
    "m": {
      level: 1,
      cities: ['Montréal'],
      subNodes: { /* and so on */ }
    }
  }
}
```

When asking for suggestions with the search term 'che', we start in the root by looking in the `subNodes` property if there is a property named `c`.
If so, we step to this node and do the same action with the next character of the search term.
The stop conditions are :
- There is no subnode for the current character. This means that the customer is looking for something we don't have. So we return an empty array.
- We've reached the end of the search term, then we just have to return the `cities` of the node we are on.

For performance purpose, this index is built as the server is started. This way, we prevent any latency on customer requests, even for the very first one.

### Score calculation

Score is calculated against 2 parameters : distance and name.

- Distance : The best score can be obtained between 0 to 10 km from the provided location. Beyond, we decrease score by 0.2% each kilometer.
- Name : I first wanted to decrease score by 10% each extra character of the city name compared to search term. Then I realized that it was too strong. So I decided to moderate it with a `loss factor` that equals 2 when a location is provided, 5 otherwise.

Global score is calculated by multiplying the distance score with the name score.
If no location was provided, the distance score equals 100.

### Search engine

The search engine must be initialized before you can use it.
To do that you just call the `init` function with an optionnal file path (pointing to a .tsv city file) and a callback which will be called when data has been fetched and indexed.
The search engine first create an `cityIndex` and give it to the `loadAndStoreTo` method of the `cityLoader` module.

### City index

The `cityIndex` module is responsible for holding cities in memory and to find cities by a search term.

### City loader

The `cityLoader` module is used to read the .tsv file containing the cities used is this challenge.

## Performances

### Server

It takes approximatively between 50 to 200 milliseconds to index the 5000 cities and start the server.

### Requests

Due to the choices I've made and explained above, requests are treated very quickly.
Response time is between 5 and 20 ms on the local computer.
However, these performances are not so great on heroku. It would be interesting to see if this is due to the network or a limitation on resource usage with the heroku free account.

## Difficulties

### Tests

As I choosed to load and index the cities at the server starting, I faced the problem of the tests running before my server was ready.
To fix it, I simply add a custom EventEmitter on the server object, as well as as a `ready` property.

Server side :
Server initializes the index and when this is achieved, it triggers a `ready` event 
Test side :
When the test runs, it looks if the server is ready, if not it listens for a `ready` event to be triggered by the custom EventEmitter.

### Data

The canadian territories and provinces are numbered and not named in the tsv file.
Curiously, it's not the case for the american states.
To fix that I've created a mapping array that I use only when the value of the `admin1` column is a number.

## Get started

Application can be tested [here](https://xmasclet-busbud-challenge.herokuapp.com/suggestions).

A basic form is available at `/help`. This is more friendly to use than typing an url.

## Improvements

- We could scale the application up by using cluster. This would make the application uses all availables cpu cores to handle more requests per second.
- Make the app.js code more readable by using express http server.