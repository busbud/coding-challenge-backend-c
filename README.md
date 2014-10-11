# City Search API for Busbud Coding Challenge

## Summary
My solution contains two approaches:
- The first configuration loads the dataset into memory via a file stream when the app initializes. API requests will search via the JavaScript object in memory. The file system is only read once.
- The second configuration will retrieve the data from MongoDB for every API request. Nothing is stored in memory.

The default approach is the first one. To switch to using the database, set the environment variable USE_MONGO to false. You can do this at runtime:

```
USE_MONGO=true node app
```

## Debugging
Debugging is disabled by default. To enable it set the environment variable DEBUG_MODE to true.
```
DEBUG_MODE=true node app
```

## API Test Page
I wrote a web page which consumes the API, and presents the output after each letter has been entered. When the app is started it will open up a route to the page at `/apitest`, i.e. [localhost:2345/apitest](http://localhost:2345/apitest). 

When you access the page for the first time, your browser should ask for permission to use your location. After you click Allow, your latitude and longitude should be displayed. Your results will now be scored and sorted based on proximity to your location.
![Permission](/docs/images/geolocation_permission.png?raw=true "Permission")

![With Geo-location](/docs/images/with-latitude.png?raw=true "With Geo-location")

![Screenshot](/docs/images/bos.png?raw=true "Screenshot")

## Geo-location Shortcuts
I have made it easier to test the API by embedding coordinates for 5 North American cities: Montreal, New York, Miami, Vancouver, and Los Angeles. To test, add the querystring argument `city` with the city name, i.e. `city=Miami`.

![Miami-Abing](/docs/images/miami-abing.png?raw=true "Miami-Abing")

## Demo
You can try the app now on Heroku.
- [API Endpoint](http://mighty-wildwood-7373.herokuapp.com/suggestions?q=Montreal)
- [API Endpoint Test Page](http://mighty-wildwood-7373.herokuapp.com/apitest)

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
