# AUTO COMPLETE API DOCUMENTATION

This aim of the API is to provide a list of suggestions that contains the cities that match the user request.

## How to use the API ?

The API is available in this url: https://calm-hollows-93494.herokuapp.com/

It exposes an endpoint /suggestions 

It takes three query string parameters:
 - q: the partial (or complete) search term | mandatory
 - latitude: caller location latitude | optional
 - longitude: caller location longitude | optional

Example : https://calm-hollows-93494.herokuapp.com/suggestions?q=london&latitude=39.88645&longitude=-83.44825
returns : 
```
  {"suggestions":[{"name":"London, OH, USA","score":1,"longitude":-83.44825,"latitude":39.88645},{"name":"London, KY, USA","score":0.9419231640247206,"longitude":-84.08326,"latitude":37.12898},{"name":"London, PE, Canada","score":0.9271661723102063,"longitude":-81.23304,"latitude":42.98339},{"name":"New London, WI, USA","score":0.6216573534036678,"longitude":-88.73983,"latitude":44.39276},{"name":"New London, CT, USA","score":0.5644621082128308,"longitude":-72.09952,"latitude":41.35565},{"name":"Lyndon, KY, USA","score":0.531609372893808,"longitude":-85.60163,"latitude":38.25674},{"name":"Londontowne, MD, USA","score":0.5108709577846785,"longitude":-76.54941,"latitude":38.93345},{"name":"Loudon, TN, USA","score":0.4926930256625713,"longitude":-84.33381,"latitude":35.73285},{"name":"Londonderry, NH, USA","score":0.45251862072252036,"longitude":-71.37395,"latitude":42.86509},{"name":"Lyndon, VT, USA","score":0.3801486636509325,"longitude":-72.01093,"latitude":44.51422},{"name":"Lindon, UT, USA","score":0.13364572663078014,"longitude":-111.72076,"latitude":40.34329}]}
```

## How to run the API locally ?

In order to run the API in your local machine, you need to:
 - install posgtresql server
 - clone the project
 - CREATE a ".env" file from ".env-template" file and update the values
 - run the command : npm run initdb
 - run the command : npm run start


## How it is working internally ?

- This API is based on posgtresql database. 
- In order to check the similarity between the user request and the city name in the db, I am using the similarity function of the pg_trgm extension.
- In order to check the distance between the longitude, lattitude passed by the caller and the ones of the city in the database, I am using <@> operator of the cube and earthdistance extensions.

## What could be improved ?

A caching service could be implemented (e.g. redis) to avoid making several database requests if the same information was already computed.
