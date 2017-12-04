# Julien's BusBud Challenge


## Explanation

I made the choice to get this app with the minimum numbers of external dependencies :
The suggestions are 100% served by the app without any external calls to other servers/services.

There's only one place where we need an external server : when we want to update the cities list, by calling geonames server.

At the startup of the server cities are loaded in memory from a local file, then we update the list with remote datas.

The advantages of this solution is :
* Startup is fast and suggestions are fully functional
* If the remote server is KO, everything works fine


Having cities loaded into memory is very efficient because the memory usage is small, there's no need to call other apis, databases or anything else.
For the auto-complete process, cities' global variable is only used with read access, so there's no risk to alter data.

### Suggestions

Note that suggestions are case insensitive and strings are normalized in order to avoid side effects with accents


    GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
    
```json
{
   "suggestions":[
      {
         "name":"London (CA), Toronto",
         "longitude":"-81.23304",
         "latitude":"42.98339",
         "score":0.8833333333333333,
         "distance":167
      },
      {
         "name":"London (US), New_York",
         "longitude":"-83.44825",
         "latitude":"39.88645",
         "score":0.8833333333333333,
         "distance":539
      },
      {
         "name":"London (US), New_York",
         "longitude":"-84.08326",
         "latitude":"37.12898",
         "score":0.8833333333333333,
         "distance":830
      },
      {
         "name":"Londontowne (US), New_York",
         "longitude":"-76.54941",
         "latitude":"38.93345",
         "score":0.7318181818181818,
         "distance":581
      },
      {
         "name":"Londonderry (US), New_York",
         "longitude":"-71.37395",
         "latitude":"42.86509",
         "score":0.7318181818181818,
         "distance":657
      }
   ]
}
```

Results are sorted by score desc, then when two score are equals, they are sorted by distance asc (if longitude and latitude were provided in the request)

**Additional parameters availables**



    nbr : Maximum number of results (default 5)
    ms  : Minimum score to reach for a city in order to be considered as a possible candidate (default 0.5) 

Example

    GET /suggestions?q=Mont&nbr=3&ms=0.6




## Cities management

**Hot reloading**

Reload cities from remote (geoname server).
This update could easily be scheduled in order to frequently update the list without restarting the server

    GET /reloadCities
    
**Count the cities**

Could be used to verify if everything is ok with the cities

    GET /countCities


## Possible improvements

**Suggestion algorithm**

The algorithm used to retrieve results could be optimized if we want to customize the way it retrieves results.

Maybe Elasticsearch could be used because it offers lots of possibilities to search strings with many customizations available.
Another advantage of Elasticsearch would be that it can handle a very higher number of cities than the in-memory solution I created.
The disadvantage is that it'll need more servers (at least 2 or 3 for one elasticsearch cluster)

**City loading**

The way I decided to load the cities from the url is specific (url, file, format).
In a production purpose, we'll have to make it more customizable to handle differents urls, file formats, ... in order to re-use it for other needs.




## Tests

All tests have been rewritten with the use of chai-http.

The score test has been corrected.

Some new tests have been added.

## Heroku deployment

Application has been deployed on Heroku, tests can be done there :

[jo-busbud-challenge.herokuapp.com](https://jo-busbud-challenge.herokuapp.com/suggestions?q=Mont)

For example :

    https://jo-busbud-challenge.herokuapp.com/suggestions?q=Mont






 
 


    
    