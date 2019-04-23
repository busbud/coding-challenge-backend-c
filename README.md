# Busbud Coding Challenge

- [API](#api)
  - [Suggestions Endpoint](#suggestions-endpoint)
    - [Request Parameters](#request-parameters)   
    - [Response](#response)
      - [Example response](#example-response)   
      - [Cities](#cities)   
        - [Example city](#example-city)   
- [Design Considerations](#design-considerations)
  - [Data Concerns](#data-concerns)
    - [Data Processing](#data-processing)
    - [Synchronous Data Loading](#synchronous-data-loading)
  - [Localisation](#localisation)
  - [Scoring](#scoring)
  - [Rate-limiting](#rate-limiting)
  - [Use of Express](#use-of-express)
  - [Query Errors](#query-errors)
  - [Code Style](#code-style)

# API
Only one API endpoint is currently available. 
### Suggestions Endpoint 
`/suggestions`, provides city autocompletion suggestions, given a search query. `GET /suggestions?q=partialQuery&latitude=45.5&longitude=-73.5`

#### Request Parameters: 
  - `q`: the partial (or full) query string. Required. Examples: `Montréal`, `la`, `YUL`
  - `latitude`: the decimal latitude of the client's location, to provide better suggestions. Optional. Example: `45.5` 
  - `longitude`: the decimal longitude of the client's location, to provide better suggestions. Optional. Example: `-73.5` 

#### Response: 
Response will be a JSON object with one property, `suggestions`, containing an array of suggested completion cities, ordered by descending score. `{"suggestions": [...cities]}`

##### Example response: 

    {
      "suggestions": [
        {
          "id": 6077243,
          "name": "Montréal",
          "ascii": "Montreal",
          "alt_name": [
            "Монтреал",
            "מונטריאול",
            "مونترآل",
            "モントリオール",
            "蒙特利尔",
            "몬트리올"
          ],
          "country": "Canada",
          "admin1": "Quebec",
          "population": 3268513,
          "latitude": 45.50884,
          "longitude": -73.58781,
          "easyDisplayName": "Montréal, Quebec, Canada",
          "distanceInKM": 6.932,
          "score": 1
        }
      ]
    }

##### Cities: 

Each city has a structure like so:
  - `id`: a numerical id which uniquely identifies the city in our backing data set. Number. Example: `1083394`
  - `name`: the name of the city, as spelled in that city's locale. May include accents. String. Example: `Montréal`
  - `ascii`: the name of the city, with no accents. String. Example: `Montreal`
  - `alt_names`: an array of alternative names for the city. These may include airport codes, nicknames, or names in other locales. Array of strings. Example: `["Монтреал","몬트리올"]
  - `country`: the country within which the city is located. String. Example: `Canada`
  - `admin1`: the administrative subdivision within which this city is located. Usually refers to a state, province or territory. String. Example: `Ontario`
  - `population`: the population of the city. Number. Example: `5001`
  - `latitude`: the decimal latitude of the city. Number. Example: `45.50884`
  - `longitude`: the decimal longitude of the city. Number. Example: `-73.58781`
  - `easyDisplayName`: the fully geographically-qualified name of the city, the way it might be written in an address. Unambiguously identifies the city within the results, and is the suggested presentation name. String. Example: `Montréal, Quebec, Canada`
  - `distanceInKM`: the distance in kilometers from the supplied client coordinates, if those were provided. Always present if `latitude` and `longitude` were present and well-formatted in the request paramters, never present otherwise. Number. Example: `6.932`
  - `score`: the score, a numerical rating of the quality of the suggestion. This is a number between 0 and 1, inclusive, with 1 being the highest-rated and best suggestion. Example: `0.52`

The search query is not guaranteed to be found in the `name` field, but in one of `name`, `ascii` and `alt_names`. All three fields are searched for better localisation compatibility. If displaying the search results

###### Example city: 

    {
      "id": 6077243,
      "name": "Montréal",
      "ascii": "Montreal",
      "alt_name": [
        "Монтреал",
        "מונטריאול",
        "مونترآل",
        "モントリオール",
        "蒙特利尔",
        "몬트리올"
      ],
      "country": "Canada",
      "admin1": "Quebec",
      "population": 3268513,
      "latitude": 45.50884,
      "longitude": -73.58781,
      "easyDisplayName": "Montréal, Quebec, Canada",
      "distanceInKM": 6.932,
      "score": 1
    }
 

# Design Considerations 
Note: The use of the word 'we' below is simply stylistic. The challenge code is entirely my own work.

### Data Concerns
Normally data, such as the sample cities data in this challenge, would be stored in a database. The decision not to use a database was twofold: 1) it would make it harder to update the data if it was ever updated upstream (in the Busbud repo), 2) it seemed out of scope for the challenge.

As such, all of the data pre-processing, modification and filtering was done in software, on start-up. This was done within the app itself, although processing it beforehand in a build system (such as [Gulp](https://gulpjs.com/)) would have also been a good solution, but seemed like overkill. Again, to facilitate data processing if it was ever updated upstream, the data file was never modified.    

##### Data Processing

The data had to be modified in several ways; to remove entries below a certain population (challenge requirement), to remove entries for cities which were not in the US or Canada (challenge requirement), to pre-sort the data by population, to replace country codes like `CA` with `Canada`, to replace province & state codes like `10` with `Quebec`, and to provide a fully geographically-qualified name, e.g. "Toronto, Ontario, Canada" instead of "Toronto", which is ambiguous (providing a unique name was also a challenge requirement).

There was also some pre-processing to do. Because the data in `cities_canada-usa.tsv` did not conform to [RFC 4180](https://tools.ietf.org/html/rfc4180) (who knew there was an RFC for CSV!), any line containing a field with a quote (`"`) was rejected by every CSV/TSV library that we checked. 
To get around this, we removed the quotation mark everywhere in the .tsv before processing it. 
       
##### Synchronous Data Loading 
We opted to load the data synchronously, as it reduced the number of lines of code from 34 to 3. Since the data is only loaded once, at start-up, there is no negative consequence to doing so. If this doesn't sound right to you, consider that 'require' statements are always synchronous, so there is no added impact, and there is 10x less code to maintain. An asynchronous equivalent was coded and can be viewed in commit 15e42d.   

### Localisation
We search not only the 'name' field of cities, but the `ascii` name field (allowing queries like 'Montreal' to match 'Montréal') as well as the `alt_name` field of the data.
 
Searching the `alt_names` field allows us to match queries with names of cities in other languages, for example, "Нью-Йорк" or "نيويورك" will both suggest New York City. While this approach is ok, it also has some drawbacks: searching for partial string "La" will unexpectedly suggest Longueuil, as one of its alternative names is "lang ji er".
 
A better approach would be to separate alternative names by locale (using CLDR data) and to accept a locale from the client. This would still suggest matches from localised names and would avoid unexpected matches from other locales. It also, and more importantly, would allow localisation of province/state/administrative region and country names. This was not implemented in this challenge for a few reasons: 1) full localisation data was not readily available (loading CLDR data, even fragments, requires a few gigs and adds considerable overhead), 2) the complexity of a localisation-indexed structure of names was best handled in a database. See [above](#data-concerns) for reaons why a database was not implemented in this challenge.

Several localisation tests have been added to `suggestions.js` 


### Scoring
To provide good matches, it was important to include distance. 
A score that goes as the inverse of a power of the distance and as a power of the population seemed to yield good results. 
A query of "la" with a lat/long of 45.5,-73.5 (roughly Montreal, technically Saint-Lambert on the south shore) was used as a litmus test. 
Using the current scoring system, Laval (2nd) appears in the results just above Los Angeles (3rd). The top result is Longueuil, for reasons explained in the 'localisation' section, above.      

### Rate-limiting
The challenge included a requirement that "mitigations to handle high levels of traffic should be implemented". This seemed most convenient to do as an Express middleware, although we acknowledge that this might best be done in a reverse-proxy, load-balancer or through the use of a service like CloudFlare.  

### Use of Express
While there was only one route, the use of [Express.js](http://expressjs.com) over Node's vanilla HTTP server seemed justified. 
  - It provides automatic URL decoding (e.g. of UTF-8 query parameters such as `?q=%D0%9D%D1%8C%D1%8E`, which decodes to `?q=Нью`) as well as parameter detangling. These could also have been achieved using Node's built-in [URL](https://nodejs.org/dist/latest-v12.x/docs/api/url.html) class, but required slightly more work.
  - It made it easy to plug in middleware, such as the rate-limiting middleware mentioned [above](#rate-limiting).

### Query Errors
In the event of invalid lat/long, an exception is generated, which is automatically bubbled up by Express into an HTTP 500 response to the client. 
Since a 500 is the correct response to invalid input, no effort was expended in changing this, nor was any bloat added to apply additional validation. A nice to have feature would be to remove the stack returned by the 500 error and to add an informative (and human-readable) message for the client, depending on how 500 was not handled by a reverse-proxy in a production environment.  

### Code Style
Although I did see the [Busbud style guide](https://github.com/busbud/js-style-guide), it was enough of a departure from js norms (specifically, the choice of snake_case for variables), that if Busbud didn't actually use this style, my code would look very particular. 
The choice was therefore made to use the very standard [semistandard](https://github.com/Flet/semistandard) code style. 
The plan is still to implement Busbud style in another branch.         
