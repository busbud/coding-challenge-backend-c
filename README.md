# Busbud Coding Challenge Backend C

## MongoDB setup
The city data is searched for in MongoDB. If you don't have a local MongoDB installation, the instructions 
If you don't have MongoDB you can find the full instructions at [docs.mongodb.org/manual/installation](http://docs.mongodb.org/manual/installation). If you are on a Linux systems, the commands below worked for me.
``` 
sudo apt-get update
sudo apt-get install -y mongodb-org
```
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

Confirm the collection by logging into Mongo. Run the commands below to ensure the database setup is correct.
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


## API Test Page
I wrote a web page which consumes the API, and presents the output after each letter has been entered. When the app is started it will open up a route to the page at `/apitest`, i.e. [localhost:2345/apitest](http://localhost:2345/apitest). 

When you access the page for the first time, your browser should ask for permission to use your location. After you click Allow, your latitude and longitude should be displayed. Your results will now be scored and sorted based on proximity to your location.
![Permission](/docs/images/geolocation_permission.png?raw=true "Permission")

![With Geo-location](/docs/images/with-latitude.png?raw=true "With Geo-location")

![Screenshot](/docs/images/API-Screenshot-Z.png?raw=true "Screenshot")

## Heroku Deployment
You can see the code in action as deployed on Heroku.
- API Endpoint: [mighty-wildwood-7373.herokuapp.com/suggestions?q=Mont](http://mighty-wildwood-7373.herokuapp.com/suggestions?q=Mont)
- API Endpoint Test page: [mighty-wildwood-7373.herokuapp.com/apitest](http://mighty-wildwood-7373.herokuapp.com/apitest)

## Geo-location Shortcuts
I have made it easier to test the API by embedding coordinates for 5 North American cities: Montreal, New York, Miami, Vancouver, and Los Angeles. To test, add the querystring argument `city` with the city name, i.e. `city=Miami`.

![Miami-Abing](/docs/images/miami-abing.png?raw=true "Miami-Abing")

## Additional Tests
I added some additional tests. The first section tests for a valid city with non-ASCII characters, i.e. Québec. Searches for Québec/Québec or Montréal/Montréal should return the same results. The other test confirms that cities will be scored by the user's location, closest cities presented first with the score approaching 1. 

Without geo-location, results will be sorted alphabetially and not scored. In a future version, I could try to use other ways to get location such as via the user's IP address.

Below are the results of `npm test` for the new items. They are mixed with some debug output from util.log statements.
```
    valid city with non-ASCII characters (Québec)
10 Oct 16:11:17 - { q: 'Québec' }
10 Oct 16:11:17 - 1 results returned for Québec
      ✓ returns a 200 
      ✓ returns an array with 1 suggestion 
    Cities scored by location to user
10 Oct 16:11:17 - { q: 'Mont', latitude: '45.496784', longitude: '-73.574124' }
10 Oct 16:11:17 - 37 results returned for Mont
      ✓ returns a 200 
      ✓ returns an array with 37 suggestions 
      ✓ first array element is Montréal 
```