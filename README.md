# Busbud Coding Challenge
[Challange Requirements](https://github.com/busbud/coding-challenge-backend-c).

Challenge solved with PHP 8.0 and Lumen framework without sql.
I used nginx for web server and docker for easy installation.

## Installation

First you need to run docker containers
```shell
$ docker-compose up -d
```
*Make sure port 80 is open and useable for nginx*

After that if everything is ok then you should get 200 response for below curl request

```shell
$ curl -IL http://localhost
HTTP/1.1 200 OK
Server: nginx/1.19.6
Content-Type: text/html; charset=UTF-8
Connection: keep-alive
X-Powered-By: PHP/8.0.0
Cache-Control: no-cache, private
Date: Sun, 31 Jan 2021 08:20:08 GMT
```
(Optionally)
If you want, you can connect the php container for configure lumen framework and run php artisan commands

```shell
$ docker-compose exec city_autocomplete bash
```

## Testing

Tests written with phpunit, inside **test** directory.
You can use commands below to run tests *(First you need to connect docker container bash for run phpunit)*
```shell
$ docker-compose exec city_autocomplete bash
$ vendor/bin/phpunit
```

## Usage
There is only one endpoint in this project
```text
GET http://localhost/suggestions
```

_This endpoint only accept GET http requests._

There is 3 parameters you can use

- **q** : is search term **(\*Required)**
- **latitude** : for location based suggestions **(\*Required only if longitude present)**
- **longitude** : for location based suggestions **(\*Required only if latitude present)**

Examples:
- Partial search term with location
```text
GET http://localhost/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
```
In this example, i give a location inside canada, so the suggestions are improved by that information and as you can see, london,canada is in the first place
```json
{
  "suggestions": [
    {
      "name": "London, ON, Canada",
      "latitude": "42.98339",
      "longitude": "-81.23304",
      "score": 0.9
    },
    {
      "name": "London, OH, USA",
      "latitude": "39.88645",
      "longitude": "-83.44825",
      "score": 0.8
    },
    {
      "name": "London, KY, USA",
      "latitude": "37.12898",
      "longitude": "-84.08326",
      "score": 0.8
    },
    {
      "name": "Hondo, TX, USA",
      "latitude": "29.34746",
      "longitude": "-99.14142",
      "score": 0.6
    },
    {
      "name": "Lindon, UT, USA",
      "latitude": "40.34329",
      "longitude": "-111.72076",
      "score": 0.5
    },
    {
      "name": "Longmeadow, MA, USA",
      "latitude": "42.0501",
      "longitude": "-72.58287",
      "score": 0.5
    },
    {
      "name": "Lyndon, VT, USA",
      "latitude": "44.51422",
      "longitude": "-72.01093",
      "score": 0.5
    },
    {
      "name": "New London, WI, USA",
      "latitude": "44.39276",
      "longitude": "-88.73983",
      "score": 0.5
    },
    {
      "name": "Lyndon, KY, USA",
      "latitude": "38.25674",
      "longitude": "-85.60163",
      "score": 0.5
    },
    {
      "name": "Orlando, FL, USA",
      "latitude": "28.53834",
      "longitude": "-81.37924",
      "score": 0.5
    }
  ]
}
```
- Full search term with location
```text
GET http://localhost/suggestions?q=London&latitude=37.98339&longitude=-84.23304
```
If i give USA location the results will change and london,USA is in the first place now
```json

  "suggestions": [
    {
      "name": "London, KY, USA",
      "latitude": "37.12898",
      "longitude": "-84.08326",
      "score": 1
    },
    {
      "name": "London, OH, USA",
      "latitude": "39.88645",
      "longitude": "-83.44825",
      "score": 1
    },
    {
      "name": "London, ON, Canada",
      "latitude": "42.98339",
      "longitude": "-81.23304",
      "score": 0.9
    },
    {
      "name": "Lyndon, KY, USA",
      "latitude": "38.25674",
      "longitude": "-85.60163",
      "score": 0.8
    },
    {
      "name": "Loudon, TN, USA",
      "latitude": "35.73285",
      "longitude": "-84.33381",
      "score": 0.7
    },
    {
      "name": "Lyndon, VT, USA",
      "latitude": "44.51422",
      "longitude": "-72.01093",
      "score": 0.7
    },
    {
      "name": "Lindon, UT, USA",
      "latitude": "40.34329",
      "longitude": "-111.72076",
      "score": 0.7
    },
    {
      "name": "New London, WI, USA",
      "latitude": "44.39276",
      "longitude": "-88.73983",
      "score": 0.6
    },
    {
      "name": "Hondo, TX, USA",
      "latitude": "29.34746",
      "longitude": "-99.14142",
      "score": 0.5
    },
    {
      "name": "Longmont, CO, USA",
      "latitude": "40.16721",
      "longitude": "-105.10193",
      "score": 0.5
    }
  ]
}
```

For live test you can use server below

