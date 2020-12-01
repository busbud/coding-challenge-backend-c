
## Geoname Suggestion API

The API is about to search the cities of USA and Canada with city name, latitude and longitude data. Search results scored and listed by name similarity and distance. 


Geoname data source: [geonames.org](https://download.geonames.org/export/dump/)

**Install**

Run in the project directory :

<pre><code>npm install
composer install
</code></pre>

**Sample Request:**

<pre><code>GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163</code></pre>

**Sample Response:**

<pre><code>{
     "suggestions": [
         {
             "name": "London, 08, Canada",
             "latitude": 42.98339,
             "longitude": -81.23304,
             "score": 0.9
         },
         {
             "name": "London, OH, USA",
             "latitude": 39.88645,
             "longitude": -83.44825,
             "score": 0.7
         },
         {
             "name": "London, KY, USA",
             "latitude": 37.12898,
             "longitude": -84.08326,
             "score": 0.5
         },
         {
             "name": "Londontowne, MD, USA",
             "latitude": 38.93345,
             "longitude": -76.54941,
             "score": 0.4
         },
         {
             "name": "Londonderry, NH, USA",
             "latitude": 42.86509,
             "longitude": -71.37395,
             "score": 0.4
         },
         {
             "name": "New London, CT, USA",
             "latitude": 41.35565,
             "longitude": -72.09952,
             "score": 0.4
         },
         {
             "name": "New London, WI, USA",
             "latitude": 44.39276,
             "longitude": -88.73983,
             "score": 0.4
         }
     ]
 }</code></pre>


**Testing**

<pre><code>php artisan test</code></pre>
