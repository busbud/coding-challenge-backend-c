BusBud Challenge
======================
### Overview
APP Deployed on heroku: https://bb-cities.herokuapp.com/<br>
API : https://bb-cities.herokuapp.com/suggestions<br>
**Example:**  https://bb-cities.herokuapp.com/suggestions?q=montreal&latitude=44&longitude=-73<br>
<br>
Code Repository:  https://github.com/rjacquemin/coding-challenge-backend-c<br>
Build status: https://circleci.com/gh/rjacquemin/coding-challenge-backend-c<br>
<br>
The code is commented and the documentation below covers most of the high level concepts.<br>
For this challenge I tried to keep the API as simple as possible, fast and reliable and to structure the code for reusability and maintainability.
I focus on the backend part and also make sure unit test cover all the modules.

### App Structure
```
.
+--app.js
+--doc.md
+--package.json
+--README.md
  +--data
  |    +-- cities_canada-usa.tsv
  +--lib
  |    +-- dataParser.js
  |    +-- scorer.js
  |    +-- search.js
  |    +-- util.js
  +--test
  |    +--scorer.js
  |    +--suggestions.js
  |    +--util.js
```

### Loading Data
lib/dataParser.js is processing the TSV file once when starting the server.  
Only necessary information are loaded in memory.
For reading file and selecting column, I am using module d3-dsv (https://github.com/d3/d3-dsv)
For performance purpose, it is important to not parse the TSV file at every request.

### Search
lib/search.js is a controller called at every request, it return an array of city suggestion order by descending score.
Exclude all cities with population less than 5000.
Return all the suggestion with a score > 0

### Scorer
lib/scorer.js is a scoring service, it is called for every city and calculate a pertinence score based on the criteria described below.

1. **Name score:**
 I decide to use module 'string-score' as text-matching mechanism.  https://www.npmjs.com/package/string-score
 This module is lightweight and fast, for each city it calculate a matching score between the given string and and the citi name.
 It does not check only the first caracters but also work if the query string is part of the name or at the end.
 The result is a number between 0 and 1 indicating how well query matches target (0 being no match and 1 being a perfect match).

2. **Geographical distance:**
If score == 0, means no match found by name,  for performance reason we dont consider geographical data.
If score > 0  after the first step and the latitude and longitude parameters are provided.
We calculate the distance between the given point and the city coordinates using the geolib module (https://github.com/manuelbieh/Geolib).
In order to improve relative scores, we apply penalty on the name score.
If the city is within 20km no penalty is applied. We then apply a penalty progressively up to 0.5 when the city is over 10000km away.  

### Util
This library provide utility function that can be use in the other module (dataParser, search, scorer )

### Tests
Functionnal test of the App  are ok, I added some more functionnal test.<br>
Unit tests, scorer and util modules are tested with separated unit test.<br>

### Possible improvements
For this challenge I decide to keep the server as simple as possible but there are many improvement that can be done :
  - Use express to build a small front end page with a textbox and using our suggestion endpoint
  - Use a nosql db to store the cities information (mongoDB / redis / ... )
  - Add limit parameter to the API to return max number of suggestions
  - Cache API results. The system could cache the full JSON response for a given request based on the parameters.
    It should be considered for a really high traffic solution.
  - Use docker to encapsulate application
