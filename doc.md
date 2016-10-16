BusBud Challenge
======================
### Overview
APP Deployed at: https://bb-cities.herokuapp.com/<br>
API : https://bb-cities.herokuapp.com/suggestions
      https://bb-cities.herokuapp.com/suggestions?q=montreal&latitude=44&longitude=-73

Repository:  https://github.com/rjacquemin/coding-challenge-backend-c<br>
Build status: https://circleci.com/gh/rjacquemin/coding-challenge-backend-c

The functional tests all pass and some more were added.
I use Circle CI for continuous integration.

The code is commented and the documentation below covers most of the high level concepts.

### Scoring ([Source code](https://github.com/Chris911/coding-challenge-backend-c/blob/master/scorer.js))

The score for every city is calculated individually based on the criteria described below.
I tried to make the scoring meaningful without doing too much calculation since I didn't want each request to spend too much time doing the score calculations and because my first goal for the challenge was to keep the API fast.

Every city starts with a score of 1.0 and penalties are applied based on the following calculations:

1. **Name score (prefix vs city name):**
Based on the given prefix (q query string), we give a penalty based on the number of missing letters needed to get the full city name.
If 3 or less letters are missing not penalty is given. The maximum penalty is 0.10 when 10 letters or more are missing. This one probably needs some as cities with longer names have a slight disadvantage.

2. **Geographical distance:**
If the latitude and longitude parameters are provided we calculate the distance between the given point and the city coordinates using the [Haversine formula](http://en.wikipedia.org/wiki/Haversine_formula). If the city is within 15km no penalty is applied. We then apply a penalty progressively up to 0.7 when the city is over 10000km away.  


### Possible improvements
For this challenge I decide to keep the server as simple as possible but there are many improvement that can be done  
  - Use Express web application framework if we want to use some modules.
  - Build a small front end page with a textbox and using jquery autocomplete
  - Using a nosql db to store the cities information (mongoDB / redis / .. )
  - Cache API results. The system could cache the full JSON response for a given request based on the parameters. This way the second request of 2 requests with the same parameters would be slightly faster as the we wouldn't need to calculate the score again. However, since the scoring calculations are fast and the data is already stored in redis with a fast retrieval time, the improvement using this technique was really small and not worth it in my opinion. It should still be considered for a really high traffic solution.  
  - Better scoring. If scoring is actually a top priority here the scoring algorithms should not only score each city individually but also against the other cities in the result set provided.
