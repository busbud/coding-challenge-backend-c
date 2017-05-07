# Busbud Coding Challenge

Here's my take on the Busbud backend coding challenged as presented in [CHALLENGE.md](CHALLENGE.md). I did not intend to develop a world-changing solution for this problem. After all, aren't scoring algorithms the holy grail of imperfect solutions ? I sure could have gone ahead and used scoring libraries or more widely-used solutions such as an ElasticSearch service. Instead, I thought it would be interesting to face this problem on an new angle that represents who I am.

## Solution

The solution was based on a framework I love which is ExpressJs. I used MongoDB for storage. Some might argue that using a database for such a small amount of data would only lead to more overhead work but I tried to keep scalability in mind. That being said, I delegated a significant part of the necessary computations through the MongoDB aggregation pipeline, from which we got a bunch of new and exciting features in [3.4](https://docs.mongodb.com/manual/release-notes/3.4/#aggregation). Of course there is still some calculations done on the server's side but I tried to keep it lightweight which can be challenging at times when aiming for availability on a single-threaded server.

### Application structure

I embrace modularity as much as I can; for maintenance purposes and especially for my own sanity; so here is how it goes:

* **app:**
    App files

* **app.models:**
    Mongoose's models definitions

* **app.routes:**
    Main routing logic

* **app.routes.api:**
    Implementation of the API routing functions

* **app.routes.controllers:**
    Core of the requests processing

* **app.routes.middlewares:**
    Middlewares such as response handlers and request validation

* **app.utils:**
    Miscellanous utility functions

* **config:**
    Talks for itself

* **data:**
    Cities data for seeding purposes

* **test:**
    Test cases

### Name scoring

I originally intended to improve the implicit requirements asking for a 'name starts with' searching feature with a 'name contains' but eventually found out that it could not be used along Mongo's powerful indexing features, so I stayed along the requirements' lines.

So how could one give a confidence score on a bunch of partially matched city names' results ?

#### Completion scoring

My initial intuition was to test the partial query's length against the result's length to establish a match completion percentage. That way, the server would basically return a 100% score for a perfect match and a 50% score when only the first half of the full city's name would be match by the query. I slept on that and later realized it did not made a lot of sense for it did not really matter what portion of the target cities were matched by the user's query. Indeed, even if we consider an actual query as having more "confidence" than a future one holding more characters i.e. the user is less likely to type any more character, it really does not say anything about what our user is looking for (that is, when returning more than a single result). The completion scoring should therefore only account for a very slight confidence boost in the overrall score.

#### Number of matches scoring

I went ahead and added a number of matches name scoring weight, which is as you might have guessed a score inversely proportional to the total number of results. It seems it makes a whole lot of sense to base a confidence level on the actual number of returned results: the more there are, the less confident we are about the validity of each and every one of those. I got good results at a weighted 90% with the remaining 10% portion established from the completion scoring explained above.

#### Optional geographic coordinates

By using GeoJSON objects within my database's documents, I was able to use some decent features of MongoDB such as $geoNear computing distance in spherical coordinates (but ignoring ellipsoidal effects). I went throught a bunch of data when using these distances along with a corresponding score obtained through a linear regression. However, I found out that these results were not exactly what I was hoping for. What happens using such a linear bijection between a distance and a score is that the difference between a 10 km radius and a 100 km radius turns out the same as the difference between a 3000 km and a 3100 km radius. The inherent problem with the confidence levels for this approach is that they do not reflect how much *more* confident we ought to be about the 10 km match in comparison to the 100 km match as we are in the 3000 km one compared to the 3100 km. The latters being pretty much the same while the difference at smaller scales should definitely be reflected in the score. For this reason, I opted for a solution based on a log scale. That way, the confidence levels won't be about the actual distance anymore but really about the *magnitude* of the distance, as shown in the following figure:

![alt tag](http://res.cloudinary.com/retrogamerz/image/upload/c_scale,w_400/v1494132311/joel/misc/logscale_radiuses.png)

I then proceeded to establish a logarithmically spaced number of bins based on distance ranges between the given reference point and the matched cities, which went like a charm utilizing Mongo's $bucket command within the pipeline. For scoring, I basically uniformally spread the results of each bin through a corresponding chunk of score range, which leaves room for future improvements.

Since the optional geographic coordinates reference are especially relevant to score results along a basic 'name starts with' query, I weighted it 90% with the remaining 10% coming for the combination of both name-scoring weights.

### High availability

I tried to fulfill the high-availability requirements through memory caching. Future work might include a second layer of high TTL caching in database. Moreover, having delegated much of the spherical coordinates calculations to my database, my server has little processing to do which is always a significant availability factor working with NodeJS. Of course, having some real-world usage plans for this server could lead to many deployment improvements. To name a few, we could do load balancing and elastic cloud. Furthermore, since our database is not really subject to frequent changes, we sure could use intermediary caching proxies of very high TTL to reduce load. A choice of geographic zones based on demand for our servers locations could equally improve performances.

### Notes

- I did not restrict the number of results since the API usage context is unknown
- See https://coding-challenge-c.herokuapp.com/suggestions for production

### Sample responses

**Match with name**

    GET /suggestions?q=Tuc

```json
{
    "status": {
        "success": true
    },
    "suggestions": [{
            "name": "Tucker, GA, US",
            "latitude": 33.85455,
            "longitude": -84.21714,
            "score": 0.2
        }, {
            "name": "Tucson, AZ, US",
            "latitude": 32.22174,
            "longitude": -110.92648,
            "score": 0.2
        }, {
            "name": "Tuckahoe, VA, US",
            "latitude": 37.59015,
            "longitude": -77.55638,
            "score": 0.19
        }, {
            "name": "Tuckahoe, NY, US",
            "latitude": 40.95038,
            "longitude": -73.82736,
            "score": 0.19
        }, {
            "name": "Tucumcari, NM, US",
            "latitude": 35.17172,
            "longitude": -103.72497,
            "score": 0.18
        }, {
            "name": "Tucson Estates, AZ, US",
            "latitude": 32.18758,
            "longitude": -111.09093,
            "score": 0.17
        }
    ]
}

```

**Near match with name**

    GET /suggestions?q=Van&latitude=48.4284&longitude=-123.3656

```json
{
    "status": {
        "success": true
    },
    "suggestions": [{
            "name": "Vancouver, BC, CA",
            "latitude": 49.24966,
            "longitude": -123.11934,
            "distance": 93.18501258116879,
            "geoScore": 1,
            "nameScore": 0.12,
            "score": 0.91
        }, {
            "name": "Vancouver, WA, US",
            "latitude": 45.63873,
            "longitude": -122.66149,
            "distance": 315.100342960414,
            "geoScore": 0.83,
            "nameScore": 0.12,
            "score": 0.76
        }, {
            "name": "Vandenberg Village, CA, US",
            "latitude": 34.70832,
            "longitude": -120.46766,
            "distance": 1545.9507699446865,
            "geoScore": 0.64,
            "nameScore": 0.11,
            "score": 0.59
        }, {
            "name": "Van Nuys, CA, US",
            "latitude": 34.18667,
            "longitude": -118.44897,
            "distance": 1636.9153741557227,
            "geoScore": 0.61,
            "nameScore": 0.13,
            "score": 0.56
        }, {
            "name": "Van Buren, AR, US",
            "latitude": 35.43676,
            "longitude": -94.34827,
            "distance": 2779.487117658044,
            "geoScore": 0.57,
            "nameScore": 0.12,
            "score": 0.53
        }, {
            "name": "Vandalia, IL, US",
            "latitude": 38.9606,
            "longitude": -89.09368,
            "distance": 2923.2665630026204,
            "geoScore": 0.56,
            "nameScore": 0.13,
            "score": 0.52
        }, {
            "name": "Van Wert, OH, US",
            "latitude": 40.86949,
            "longitude": -84.58412,
            "distance": 3147.494805887777,
            "geoScore": 0.55,
            "nameScore": 0.13,
            "score": 0.51
        }, {
            "name": "Vandalia, OH, US",
            "latitude": 39.89061,
            "longitude": -84.19883,
            "distance": 3229.518279221104,
            "geoScore": 0.54,
            "nameScore": 0.13,
            "score": 0.5
        }, {
            "name": "Vandergrift, PA, US",
            "latitude": 40.60284,
            "longitude": -79.56477,
            "distance": 3532.5123847858163,
            "geoScore": 0.52,
            "nameScore": 0.12,
            "score": 0.48
        }, {
            "name": "Vancleave, MS, US",
            "latitude": 30.54047,
            "longitude": -88.68752,
            "distance": 3536.959872190803,
            "geoScore": 0.51,
            "nameScore": 0.12,
            "score": 0.47
        }
    ]
}

```