# Busbud coding challenge

## Online Api

https://busbud-test.herokuapp.com/suggestions?q=Montreal

## Installation

* Install nodejs environment and start
* Install Redis and start

## Usage


GET /suggestions?q=Montreal

```json
{
   "suggestions":[
      {
         "name":"Montreal, Quebec, Canada",
         "latitude":"45.50884",
         "longitude":"-73.58781",
         "score":1
      },
      {
         "name":"Montrose, Virginia, United States",
         "latitude":"37.5207",
         "longitude":"-77.37831",
         "score":0.7
      },
      {
         "name":"Montrose, Colorado, United States",
         "latitude":"38.47832",
         "longitude":"-107.87617",
         "score":0.7
      },
      {
         "name":"Montvale, New Jersey, United States",
         "latitude":"41.04676",
         "longitude":"-74.02292",
         "score":0.7
      },
      {
         "name":"Monterey, California, United States",
         "latitude":"36.60024",
         "longitude":"-121.89468",
         "score":0.7
      },
      {
         "name":"Mont-Royal, Quebec, Canada",
         "latitude":"45.51675",
         "longitude":"-73.64918",
         "score":0.6
      }
   ]
}
```

GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

```json
{
   "suggestions":[
      {
         "name":"London, Ontario, Canada",
         "latitude":"42.98339",
         "longitude":"-81.23304",
         "score":0.9
      },
      {
         "name":"London, Ohio, United States",
         "latitude":"39.88645",
         "longitude":"-83.44825",
         "score":0.89
      },
      {
         "name":"London, Kentucky, United States",
         "latitude":"37.12898",
         "longitude":"-84.08326",
         "score":0.89
      },
      {
         "name":"Loudon, Tennessee, United States",
         "latitude":"35.73285",
         "longitude":"-84.33381",
         "score":0.79
      },
      {
         "name":"Longueuil, Quebec, Canada",
         "latitude":"45.53121",
         "longitude":"-73.51806",
         "score":0.39
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
* With the levenshtein calcul distance between 2 words : Londa is the same distance to Londo than London. We can solve this problem by proposing the correct word to the user instead of changing the calcul.
* This solution suppose that the probability to have 2 place names, 10 letters each which are completely different is very low; we suppose that the levenshtein distance will be < 10 (or we set it to 0).
* Solution for high levels of traffic :
  * I used Redis to cache geonames response, so we get the response faster and we don't have to do an other request to geonames api. This solution is good but it depends of the business target. We are still limited to 30000 requests per day, 2000 per hour. I choose to use Redis because it is free on Heroku, whereas Solr and other solutions are not free.
* Bunch of options for the future
  * Get csv daily, push it to Solr, realise a search in the SOlr engine to get cities by a placename and options, cache Solr responses.
  * Get csv daily, push it to Redis, or Mongodb, or Memcached, create a search engine to retrieve cities by placename.