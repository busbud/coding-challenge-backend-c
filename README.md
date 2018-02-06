Heroku URL : https://mysterious-anchorage-55896.herokuapp.com
Sample URL : https://mysterious-anchorage-55896.herokuapp.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163


Circle CI : Tests passing (https://circleci.com/gh/itssujan/coding-challenge-backend-c/tree/master)


Approach :
------------

Added expressjs support.
Used Trie Datastructure to store and search for the city information.
Added rate limitter to restrict multiple calls from a particular IP

Sample output
--------------

{
   "suggestions":[
      {
         "name":"London , 08 , CA",
         "latitude":"42.98339",
         "longitude":"-81.23304",
         "score":0.8
      },
      {
         "name":"London , OH , US",
         "latitude":"39.88645",
         "longitude":"-83.44825",
         "score":0.6
      },
      {
         "name":"Londontowne , MD , US",
         "latitude":"38.93345",
         "longitude":"-76.54941",
         "score":0.5
      },
      {
         "name":"London , KY , US",
         "latitude":"37.12898",
         "longitude":"-84.08326",
         "score":0.3
      },
      {
         "name":"New London , WI , US",
         "latitude":"44.39276",
         "longitude":"-88.73983",
         "score":0.3
      }
   ]
}
