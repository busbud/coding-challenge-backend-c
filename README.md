# City Search API for Busbud Coding Challenge

My solution contains two approaches:
- The first configuration loads the dataset into memory via a file stream when the app initializes. API requests will search via the JavaScript object in memory. The file system is only read once.
- The second configuration will retrieve the data from MongoDB for every API request. Nothing is stored in memory.

The default approach is the first one. To switch to using the database, set the environment variable USE_MONGO to false. You can do this at runtime:

```
USE_MONGO=true node app
```
## Performance Differences Between Approaches
I used two approaches because I wanted to test which one produced better performance to handle high levels of traffic. Was it more efficient to load everything into memory once, or to query a database with each request? Below is a performance chart based on the current configuration.

![Response Times](/docs/images/response-times.png?raw=true "Response Times")

The search-by-object-in-memory approach is 31% faster than the search-by-database approach. The cost of this is 385 milliseconds while the file stream loads 7237 cities into an object array (you can see this if you set `DEBUG_MODE=true USE_MONGO=false`). Of course these results would differ based on the type of database or its optimization, as well as how I'm searching the JS object. Performance would also change based on server load at the time. Thus, it is possible that a database search would be faster, especially for huge datasets or searches requiring joins of multiple datasets. A conclusive result would be out of scope of this coding challenge, but is worth pursuing.

Unfortunately, although the load-data-at-app-startup approach is faster in this limited dataset, those extra 386 milliseconds cause the initial mocha tests to fail while the data is still loading. The tests, from what I could determine, do not allow for an "app warmup" period. The assumption is that the app starts immediately and that URL requests will work. I would need to either reduce my startup time, or somehow make the app startup synchronous based on a "data is ready" callback.

Note that `USE_MONGO=true npm test` should work fine and produce successful results for all tests.
```
  GET /suggestions
    non-existent city
      ✓ returns a 404 
      ✓ returns an empty array of suggestions 
    valid city
      ✓ returns a 200 
      ✓ returns an array of suggestions 
      ✓ contains a match 
      ✓ contains latitudes and longitudes 
      ✓ contains scores 
    valid city with non-ASCII characters (Québec)
      ✓ returns a 200 
      ✓ returns an array with 1 suggestion 
    Cities scored by location to user
      ✓ returns a 200 
      ✓ returns an array with 37 suggestions 
      ✓ first array element is Montréal 


  12 passing (258ms)
```

## API Test Page
I wrote a web page which consumes the API, and presents the output after each letter has been entered. When the app is started it will open up a route to the page at `/apitest`, i.e. [localhost:2345/apitest](http://localhost:2345/apitest). 

![Screenshot](/docs/images/bos.png?raw=true "Screenshot")

When you access the page for the first time, your browser should ask for permission to use your location. After you click Allow, your latitude and longitude should be displayed. Your results will now be scored and sorted based on proximity to your location.
![Permission](/docs/images/geolocation_permission.png?raw=true "Permission")

![With Geo-location](/docs/images/with-latitude.png?raw=true "With Geo-location")

## Geo-location Shortcuts
I have made it easier to test the API by embedding coordinates for 5 North American cities: Montreal, New York, Miami, Vancouver, and Los Angeles. To test, add the querystring argument `city` with the city name, i.e. `city=Miami`.

![Miami-Abing](/docs/images/miami-abing.png?raw=true "Miami-Abing")

## Demo
You can try the app now on Heroku.
- [API Endpoint](http://mighty-wildwood-7373.herokuapp.com/suggestions?q=Montreal)
- [API Endpoint Test Page](http://mighty-wildwood-7373.herokuapp.com/apitest)

## Debugging
Debugging is disabled by default. To enable it set the environment variable DEBUG_MODE to true.
```
DEBUG_MODE=true node app
```
## MongoDB setup
If you don't have a local MongoDB installation, you can find the full instructions at [docs.mongodb.org/manual/installation](http://docs.mongodb.org/manual/installation) for your OS.

Once you have Mongo, you can import the TSV file into a collection with the command below.
``` 
mongoimport -d busbud -c cities --file data/cities_canada-usa.tsv --type tsv --headerline
```
This should produce the output below
```
connected to: 127.0.0.1
Fri Oct 10 15:18:22.951 check 9 7238
Fri Oct 10 15:18:23.943 imported 7237 objects
```

You can confirm the collection by logging into Mongo. Run the commands below to ensure the database setup is correct.
```
> show dbs
busbud	0.0625GB
> use busbud
switched to db busbud
> show collections
cities
system.indexes
> db.cities.find().count()
7237
```
## Additional Tests
I added some additional tests. The first section tests for a valid city with non-ASCII characters, i.e. Québec. Searches for Québec/Québec or Montréal/Montréal should return the same results. The other test confirms that cities will be scored by the user's location, closest cities presented first with the score approaching 1. 

Without geo-location, results will be sorted alphabetially and not scored. In a future version, I could try to use other ways to get location such as via the user's IP address.

Below are the results of `npm test` for the new items. 
```
    valid city with non-ASCII characters (Québec)
      ✓ returns a 200 
      ✓ returns an array with 1 suggestion 
    Cities scored by location to user
      ✓ returns a 200 
      ✓ returns an array with 37 suggestions 
      ✓ first array element is Montréal 
```
