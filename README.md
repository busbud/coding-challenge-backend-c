# City Microservice

  This implementation uses MongoDb.

## Database creation

First import the tsv file in a mongo database :

```
sed 's/\"//g' data/cities_canada-usa.tsv > data/cities_canada-usa.clean.tsv

mongoimport --db busbud --collection cities --type tsv --headerline --file data/cities_canada-usa.clean.tsv
mongoimport --db busbud --collection provinces --type tsv --fields code,name,ascii,geonameid --file data/admin1CodesASCII.txt
```

Then create indexes:

```
MONGO_URL="YOUR_MONGO_URL" node initdata.js
```

## Usage

A maximum of 20 results is returned in each response.

### Search by name

```
GET /suggestions?q=Montréal

{
    "suggestions": [
        {
            "name": "Montréal, Quebec, CA",
            "latitude": 45.50884,
            "longitude": -73.58781,
            "score": 1
        },
        {
            "name": "Montréal-Ouest, Quebec, CA",
            "latitude": 45.45286,
            "longitude": -73.64918,
            "score": 0.7954545454545454
        }
    ]
}
```

### Search by name part and geo coordinates

If precise geo coordinates are given, nearest matching cities have a better score.

Latitude should be between -90 and 90 and longitude between -180 and 180, otherwise an error will be returned.

Example with Mont-Tremblant coordinates:

```
GET /suggestions?q=Mo&latitude=46.21274&longitude=-74.58438

{
    "suggestions": [
        {
            "name": "Mont-Tremblant, Quebec, CA",
            "latitude": 46.21274,
            "longitude": -74.58438,
            "score": 0.4375
        },
        {
            "name": "Monson, Massachusetts, US",
            "latitude": 42.10426,
            "longitude": -72.31897,
            "score": 0.3750005083586601
        },
        {
            "name": "Monroe, New York, US",
            "latitude": 41.33065,
            "longitude": -74.18681,
            "score": 0.3750004592166842
        },
        {
            "name": "Moosic, Pennsylvania, US",
            "latitude": 41.35341,
            "longitude": -75.73825,
            "score": 0.3750004555322313
        },
        {
            "name": "Monsey, New York, US",
            "latitude": 41.11121,
            "longitude": -74.06848,
            "score": 0.375000439050676
        },
        {
            "name": "Montréal, Quebec, CA",
            "latitude": 45.50884,
            "longitude": -73.58781,
            "score": 0.30000227196475626
        },
        {
            "name": "Montague, Massachusetts, US",
            "latitude": 42.53564,
            "longitude": -72.53509,
            "score": 0.3000005674443664
        },
        {
            "name": "Montmagny, Quebec, CA",
            "latitude": 46.98043,
            "longitude": -70.55493,
            "score": 0.2727280544629094
        },
        {
            "name": "Mont-Joli, Quebec, CA",
            "latitude": 48.58388,
            "longitude": -68.19214,
            "score": 0.2727277281083712
        },
        {
            "name": "Mount Ivy, New York, US",
            "latitude": 41.18676,
            "longitude": -74.03486,
            "score": 0.27272771817957453
        },
        {
            "name": "Mont-Royal, Quebec, CA",
            "latitude": 45.51675,
            "longitude": -73.64918,
            "score": 0.2500023562120412
        },
        {
            "name": "Morristown, Vermont, US",
            "latitude": 44.55727,
            "longitude": -72.62373,
            "score": 0.25000104301507875
        },
        {
            "name": "Montpelier, Vermont, US",
            "latitude": 44.26006,
            "longitude": -72.57539,
            "score": 0.2500009314397036
        },
        {
            "name": "Monticello, New York, US",
            "latitude": 41.65565,
            "longitude": -74.68933,
            "score": 0.2500004927471387
        },
        {
            "name": "Mount Kisco, New York, US",
            "latitude": 41.20426,
            "longitude": -73.72708,
            "score": 0.2307696757834945
        },
        {
            "name": "Mont-Laurier, Quebec, CA",
            "latitude": 46.55011,
            "longitude": -75.4993,
            "score": 0.2142888523553676
        },
        {
            "name": "Montréal-Ouest, Quebec, CA",
            "latitude": 45.45286,
            "longitude": -73.64918,
            "score": 0.18750224358603204
        },
        {
            "name": "Moultonborough, New Hampshire, US",
            "latitude": 43.7548,
            "longitude": -71.39674,
            "score": 0.18750067343771773
        },
        {
            "name": "Montville Center, Connecticut, US",
            "latitude": 41.47899,
            "longitude": -72.15119,
            "score": 0.16666711156739203
        },
        {
            "name": "Mont-Saint-Hilaire, Quebec, CA",
            "latitude": 45.56678,
            "longitude": -73.19915,
            "score": 0.1500019351462081
        }
    ]
}
```

### Errors

Error responses are in json in this format:

```
{"error":"q is mandatory"}
```

```
{
    "error": "Invalid latitude"
}
```

```
{
    "error": "Invalid longitude"
}
```
 
The status code is 400

## Score

The score of each result is the weighted average mean of two different scores :

- the name score : inverse of levenshtein distance between the name of the city and the query (q)
- the geo score : inverse of distance between the city and the query (latitude and longitude). If the query does not contain latitude and longitude, the geo score is 1.

The name score has a weight of 3 and the geo score a weigth of 1. 
This can be adjusted in score.service.js (could be made configurable in a next iteration ).


## States / Provinces

The province service uses an LRU cache to avoid hitting the database for each city.

The LRU cache has a default size of 100 items, which is enough to contain all USA and Canadians provinces, and at the same time small enough to avoid potential memory consumption issues (should the service be used for every countries worldwide).

