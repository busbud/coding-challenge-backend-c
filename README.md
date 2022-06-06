# Notes and considerations

## Solution

### Pre-requisites

```
Node >=15.0.0
```

### Deployment

The API is deployed in Heroku, here

### Indexing the data, and searching

Given the nature of the requirement to provide suggestions based on a search term, indexing the data it's a must. But the indexing needs to be in a way that if I provide a partial value I can find all related items. That's how I stumbled upon **Tries**. Tries are data structured that do exactly that, store each character that forms a word so I can provide suggestions.

I implemented it using HashMaps, that way the search is constant and the data will remain unique.

Once the data is indexed, the search will take as long as the search term is.

### Performance considerations and assumptions

- **Caching**. Indexing the data plays a big part in making the search faster, but to have to do that on every search could be constly as data increases, additional to that this is information that normally does not change so it makes it a great candidate for Caching. So I decided to cache it once indexed. Right now I'm using In-memory Caching for simplicity, but Distributed Caching solution like Redis would be the preferred option in this case for the following reasons:
  - Storing the cache in a separate server provides separation of concerns, and provides more opportunities to scale.
  - In-Memory caching has a limited physical space, and increasing that physical space on the server can turn out to be more costly and probably unnecessary, so delegating that responsibility to a Distributed Caching service.
  - In-Memory caching will be reset if the server where deployed is re-started. Now, that's a normal behavior but that becomes a hurdle when you have CI/CD in place and on each deployment cached is cleared which will cause everything to be re-calculated.
- **Limiting search term**: To make sure of not having to traverse for a specific node I will only perform the suggestions search if the search term is more than 3 characters.
- **Caching big result sets**: Given that memory cache is limited, I'm only caching responses with big result-sets.
- **Asynchronous coding**: Given the fact that JavaScript is single-threaded, making sure to work with Promises and asynchronous code will prevent blocking threads and transactions.

### Scoring Algorithm

The scoring algorithm is based on the math principle called Sum of Products. Basically how the name suggests I am adding up the products that come from the Score Category and its weight. That way everything will have the importance of the weight I assigned.

I created 3 categories:

- **Search accuracy**: how accurate is the word, this is the determined by how deep do I go in the tree after the search term completes. If I go deeper means that there are other suggestions, that gives it more importance to the ones that are closer to the search term. This category has a weight of **30%**, meaning that whatever the result, this makes up 30% of the final score.
- **Location Proximity**: if latitude and longitude are provided, this determines how close or far the city is from the user's location. This category has a weight of **50%**, meaning that whatever the result, this makes up 50% of the final score. I wanted this to this most weighted category so cities that match that criteria immediately go to the top results.
- **Frequently Searched**: If a search term is constantly looked and it has a very exact result (I'm assuming if it returns one exact result), then I add this to an LRU Cache. An LRU Cache is a caching algorithm that has an eviction policy to remove all the items that haven't been recently accessed. Leaving in the cache only those who are constantly searched. This category has a weight of **20%**.

Each category result will be calculated to its respective percertage and then summed up together to make the score between 0 and 1.

### Additional Features

- Indexed the alternate names increasing the accuracy of the search tree.
- Accounts for typos when performing the search, if while doing the search I noticed that a character cannot be found, I stop the iteration there and traverse the tree from the previous successful node. That way we don't loose the complete word based on a typo.
- Provide score to the most searched items so they rank higher on the results.

### Examples

**Request partial name**

```
/suggestions?/suggestions?q=Londo
```

![partial name](/test/screenshots/partial-search.png)

**Request partial name with Location**

```
/suggestions?/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
```

![partial name](/test/screenshots/partial-search-location.png)

**Request using alternate name**

```
/suggestions?q=NYC
```

![Alternate Name Search](/test/screenshots/alternate-name-search.png)

**Request a partial term, frequently searched items are ranked first in the result.**

```
/suggestions?q=new
```

![Frequently searched](/test/screenshots/most-searched-result-first.png)

**Request a partial term with a typo**

```
/suggestions?q=londqn
```

![typo](/test/screenshots/typo-search.png)

### Tests results

```
 GET /suggestions
    with a non-existent city
      ✔ returns a 404
      ✔ returns an empty array of suggestions
    with a valid city
      ✔ returns a 200
      ✔ returns an array of suggestions
      ✔ contains a match
      ✔ scores have valid values
      Validate the shape of the data being returned
        ✔ contains latitudes and longitudes
        ✔ contains scores
    with a less-than 3 characters search term
      ✔ returns a 404
      ✔ returns an empty array of suggestions
    with no search query
      ✔ returns a 404
      ✔ returns an empty array of suggestions
    with a valid city and location
      ✔ returns a 200
      ✔ returns an array of suggestions
      ✔ contains a match
      ✔ scores have valid values
      Validate the shape of the data being returned
        ✔ contains latitudes and longitudes
        ✔ contains scores


  18 passing (231ms)
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
