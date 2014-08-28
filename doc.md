Autocomplete Challenge
======================
### Overview
Deployed at: http://busbud-autocomplete.herokuapp.com/<br>
Repository: https://github.com/Chris911/coding-challenge-backend-c<br>
Build status: [![Build Status](https://travis-ci.org/Chris911/coding-challenge-backend-c.svg?branch=master)](https://travis-ci.org/Chris911/coding-challenge-backend-c)

My main priorities for this challenge were to make the autocomplete API fast and to structure the code for reusability and maintainability. The different components (scoring, autocomplete, web requests) each have their own modules and to keep everything fast all the data is stored in memory using Redis. More details below.

The functional tests all pass and some more were added to cover the extra features I added. I use Travis CI for continuous integration.

The code is commented and the documentation below covers most of the high level concepts. Let me know if you have any questions!

### Autocomplete ([Source code](https://github.com/Chris911/coding-challenge-backend-c/blob/master/autocomplete.js))

The autocomplete system relies heavily on redis and the data structures it provides. When the `populate()` function is called, the `tsvConverter` module parses the tsv file into a JSON object containing only the information we need. I used the ascii name here to get rid of the accented letters. Note that searching with accents still works because I replace them in the query string. The cities data is then stored in a hash table in redis where the key is the city ID and the value is a JSON string containing the relevant information about the city. The other part of the system is the index table where every possible prefix is associated with one or more city IDs. This is done using an ordered set for the prefixes which themselves point to another set of IDs.

Since the retrieval time for all of the data structures used here is `O(log(N))` we are guaranteed to have great speed even with a large dataset. However, as the dataset grows the space complexity also grows linearly so more RAM is required to store the data.

This approach is a mix of multiple different approaches I found online. It adds some complexity over a [basic string autocomplete](http://oldblog.antirez.com/post/autocomplete-with-redis.html) but is also slightly less complicated [than others](http://thorstenball.com/blog/2012/06/08/search-autocompletion-with-redis/). I'll be really happy to discuss the implementation details in person if you want. See the references section below for a list of all the articles I used.

### Scoring ([Source code](https://github.com/Chris911/coding-challenge-backend-c/blob/master/scorer.js))

The score for every city is calculated individually based on the criteria described below. I tried to make the scoring meaningful without doing too much calculation since I didn't want each request to spend too much time doing the score calculations and because my first goal for the challenge was to keep the API fast. This is why each city is scored individually and not against the other cities in the set which would have required multiple more iterations over the result set.

Every city starts with a score of 1.0 and penalties are applied based on the following calculations:

1. **Geographical distance:**
If the latitude and longitude parameters are provided we calculate the distance between the given point and the city coordinates using the [Haversine formula](http://en.wikipedia.org/wiki/Haversine_formula). If the city is within 15km no penalty is applied. We then apply a penalty progressively up to 0.7 when the city is over 2000km away.  

2. **Name score (prefix vs city name):**
Based on the given prefix (q query string), we give a penalty based on the number of missing letters needed to get the full city name. If 3 or less letters are missing not penalty is given. The maximum penalty is 0.10 when 10 letters or more are missing. This one probably needs some as cities with longer names have a slight disadvantage.

3. **Population score:**
Cities with a population over 100 000 do not get a penalty. A penalty is then applied progressively up to 0.10 for cities that have less than 7500 people. A bonus of 0.05 is also applied to cities that have a population over 1 000 000.

### API ([Source code](https://github.com/Chris911/coding-challenge-backend-c/blob/master/app.js))

I used the Express web application framework for the API. It's small, fast and does everything we need for this challenge. It is also one of the most popular Node JS web frameworks so there are a lot of modules available.

1. **Suggestions**

**Path:** `GET /suggestions`<br>

**Parameters:**

| Name      | Type   | Description                                                                      |
|-----------|--------|----------------------------------------------------------------------------------|
| q         | string | The search term or prefix                                                        |
| latitude  | float  | Location latitude to improve relative score                                      |
| longitude | float  | Location longitude to improve relative score                                     |
| score     | bool   | If 'false', the score won't be calculated and every entry will have a score of 1 |
| limit     | int    | Limit response to the top X results                                              |

**Example:**<br>
`GET /suggestions?q=montr&latitude=44&longitude=-73`
```JSON
{
  "suggestions": [
    {
      "id": "6077243",
      "name": "Montreal, QC, CA",
      "latitude": "45.50884",
      "longitude": "-73.58781",
      "score": 0.9891
    },
    {
      "id": "6077265",
      "name": "Montreal-Ouest, QC, CA",
      "latitude": "45.45286",
      "longitude": "-73.64918",
      "score": 0.7507
    },
    {
      "id": "4773747",
      "name": "Montrose, VA, US",
      "latitude": "37.5207",
      "longitude": "-77.37831",
      "score": 0.6248
    },
    {
      "id": "7259400",
      "name": "Montrose-Ghent, OH, US",
      "latitude": "41.1538",
      "longitude": "-81.64378",
      "score": 0.5388
    },
    {
      "id": "5431710",
      "name": "Montrose, CO, US",
      "latitude": "38.47832",
      "longitude": "-107.87617",
      "score": 0.2191
    }
  ]
}
```
2. **Populate and clear**

These endpoints are mostly used for debugging and benchmarking purposes. [More details in code.](https://github.com/Chris911/coding-challenge-backend-c/blob/master/app.js#L100-L103)

**Paths:**<br>
`POST /suggestions/populate`<br>
`POST /suggestions/clear`

**Parameters:**

| Name      | Type   | Description      |
|-----------|--------|------------------|
| key       | string | Secret key       |

**Warning**: You can use the following CLI commands to clear and populate the production redis database. Use carefully.

```
// Clear
curl --request POST 'http://busbud-autocomplete.herokuapp.com/suggestions/clear?key=bb4af96c181317bed81ee6c61a70c23e'
// Populate
curl --request POST 'http://busbud-autocomplete.herokuapp.com/suggestions/populate?key=bb4af96c181317bed81ee6c61a70c23e'
```

### Frontend / UI
I added a simple frontend powered by the suggestions API I built to demonstrate a real-life usage scenario. It uses jQuery UI and [jQuery Autocomplete](http://jqueryui.com/autocomplete/) for the UI and the list is limited to the top 8 results using the `limit` parameter of the API.

[Live Demo](http://busbud-autocomplete.herokuapp.com/)

### Possible improvements
  - Cache API results. The system could cache the full JSON response for a given request based on the parameters. This way the second request of 2 requests with the same parameters would be slightly faster as the we wouldn't need to calculate the score again. However, since the scoring calculations are fast and the data is already stored in redis with a fast retrieval time, the improvement using this technique was really small and not worth it in my opinion. It should still be considered for a really high traffic solution.  
  - Better scoring. If scoring is actually a top priority here the scoring algorithms should not only score each city individually but also against the other cities in the result set provided.

### References
  - [Auto Complete with Redis](http://oldblog.antirez.com/post/autocomplete-with-redis.html)
  - [Ordered Search Autocompletion With Redis](http://thorstenball.com/blog/2012/06/08/search-autocompletion-with-redis/)
  - [Soulmate Ruby Gem](https://github.com/seatgeek/soulmate/)
  - [Two ways of using Redis to build a NoSQL autocomplete search index](http://patshaughnessy.net/2011/11/29/two-ways-of-using-redis-to-build-a-nosql-autocomplete-search-index)
