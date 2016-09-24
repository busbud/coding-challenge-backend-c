# Design Notes

In practice, implementing a location suggestion service would usually involve:

1. Taking a quick peek at search logs to see the search terms people are searching for, especially the ones that return no results (or incorrect results). This would be a starting point for the design of feature, to determine what kind of solution we want: How many searches are we getting and what volume we should expect from search suggestions? Do we need fuzzy search because users are typing a lot of errors? What are the percentages of people searching for large cities vs small cities? What percentages of queries would have lat/long info? 

2. For this exercise, since we do not have any of the search volumne data, we can try to make something that is relatively simple and quite quick to test out against company employees (alpha) and small set of external users (beta). 

3. The implementation here is a simple prefix match on normalized query and cities (and their aliases). There are no fuzzy searching here for the sake of simplicity. If there is a real need for fuzzy search (based on search volume data), chances are it would be better to install some third-party software to handle it (such as Lucene or Elastic Search). Configuring, maintaining, and provisioning these third-party software can be quite a bit of work especially when you do not already have it setup, so we should be sure we need it before we install it.

4. We prune out the list of cities to match on based on the first character. This should be more than sufficient for the list of available cities/aliases since the list is relatively small. If the list gets bigger, then we may construct a trie based on the prefix names to yield a quicker search (but that would be more complicated to implement, and given how small the current list is, you would not get much improvement out of this anyways).


## Profiling, runtime, and scaling

The input data file is 1.1MB, which should easily fit into the server memory even if each entry had a bunch of additional overhead. Given the current scope of locations, we can just store all of this inside memory and not bother with setting it up in an external service or server (such as memcache). As the data size grows, or if we need to scale across a lot of servers, then it would make sense to pull this data and the suggestion service out to a smaller set of dedicated servers instead.

A simple process.memoryUsage() before and after the call to init() shows that we end up using approx 12.5MB of extra memory with the data. This is not a lot of memory for the server and should not be something to worry about (unless our data set needs to grow a lot), and most of the overhead likely comes from the code precomputing all the normalized names and aliases to increase performance at the expense of a little more memory.

I did a simple benchmark for a run on my machine, and most suggestions returned in under 10ms (6000 req/s), with the highest one around 15ms (4000 req/s). Depending on the actual server utilization/load/machines, we may need to continue investing in optimizing it. Generally speaking, I would say this is good enough and it is not worth spending a lot of time into optimizing it (easier to just provision more machines).


## Pushed to Heroku

The code has been pushed to:
https://hidden-depths-37163.herokuapp.com/suggestions?q=montreal&latitude=45.5017&longitude=-73.5673


## Scoring criteria

* The main criteria is prefix matching on the name (and aliases) of the city, with 0.7 of the weight. If there is prefix mismatch in terms of naming, we immediately return a score of 0 rather than computing the rest.
* Compute a non-relative score that is consistent over different calls, instead of a relative score amongst cities that match
* Distance is allocated a weight of 0.3, but it cuts off once the query lat/long is just two time zones away from the city
* Making the Inference that a user is more likely to search for more populous cities, we give population a weight of 0.2
* The weights add up to more than 1, but maxes out at 
* In practice, it is probably better to look into scoring suggestions based on search volume/history rather than on just city data
