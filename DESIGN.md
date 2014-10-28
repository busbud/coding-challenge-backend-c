# Solution Design

This document explains some of the design choices made when building the solution.

## Design

### Structure

The solution is split into two modules: the basic `app` module will handle the web-tier portion, while the `cities`
module will encapsulate accessing the city data.

The `cities` module offers one option to load the data, through the file provided in the project. Additional methods
to load data from alternative source (e.g. the [Geonames Web Services](http://www.geonames.org/export/web-services.html)
might be added later.

All module use asynchronous operations, as much as possible, to maximize it's capacity to handle traffic.

### Persistence

[Redis](http://redis.io/) was selected for persisting the list of cities for several reasons:

* IO throughput: The solution is expected to serve a high level of traffic and Redis offers exceptional IO throughput.
The existence of an optimized binary client for Node ([hiredis](https://github.com/redis/hiredis-node) should provide 
additional performance benefits.

* Ease of deployment: Since the solution is targeted towards deployment on Heroku, Redis is easily made available 
through an Heroku add-on ([RedisToGo](https://devcenter.heroku.com/articles/redistogo)), which makes for an easy 
deployment.

* Possibility of loading from snapshot: Compared to purely volatile in-memory caches (like memcached), Redis can be 
loaded with a snapshot of data on startup, meaning other instances could be spun up with the existing data. This might
come in handy if moving away from Heroku, in order to minimize disruption.

### Web-tier

[Express](http://expressjs.com/) was used for HTTP request routing in the web-tier under the assumption that it is also
the framework in use for other services. This has the following benefits:

* Consistency: Using the same framework will help maintenance.

* Code reuse: Error handlers and other middleware could be reused in this solution.

* Abstraction: Express provides handy abstractions for handling JSON data in a clear manner.

### Scoring algorithm

The solution uses a scoring algorithm for the list of cities matching the query that assigns weights to criteria.
For each criteria, a city scores a percentage of that weight. The sum of the score will be between 1 and 0.

The criteria are:

##### Query criteria

The city whose name fully matches the query scores 100%, while cities whose name is not completely matched score a lower
percentage, proportional to the number of unmatched characters in their name.

This criteria is based on the expectation that cities matching exactly the query are more likely to be a match.

##### Population criteria

The city with the largest population in the list scores 100%, while smaller cities score a lower percentage, proportional
to their population.

This criteria is based on the expectation that larger cities are more popular and are more likely to be a match.

##### Distance criteria

This criteria in only used when a latitude and longitude is provided in the request. The city closest to that location
scores 100%, while cities farther away score a lower percentage, proportional to their distance.

This criteria is based on the expectation that the user will look for cities near them.


## Limitations

The solution has the following limitations:

##### City names are matched from the start

While this will work for a vast majority of cities, it will fail in some cases (e.g: Valleyfield, in Canada, is the name
commonly used for Salaberry-de-Valleyfield). 

##### When location is provided, cities close to it are favored

This may seem like an intuitive thing to do, but absent information on how the users will call the service,
it might turn out that users planning a trip in a foreign country will get nearby cities returned, instead of those
expected.


## Further improvements

These are potential improvements to the solution that might be added at a later time:

##### Improve search by matching anywhere in the name

This would slow down the request, but might provide a better list of results.

##### Improve speed by setting a minimum length of query

The solution does not require a minimum length to the query parameter. Enforcing a two or three characters minimum

##### Improve speed by caching location-less searches

Searches performed without locations will return the same results, unless the underlying data changes. Caching at the
HTTP level (using Varnish or similar services) might offer interesting improvements.

##### Improved criteria weighting

Analytics of user queries and their city selection might indicate if the weighting assigned to each criteria in the
scoring algorithm is correct or needs to be adjusted.
