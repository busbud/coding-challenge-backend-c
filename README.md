# Busbud Coding Challenge

This repository is for my take on the javascript API coding challenge from Busbud, built on
[ExpressJs](https://expressjs.com/) backed by a MongoDB database with some configuration files being based off [MEAN.JS](https://github.com/meanjs/mean).

The implemented solution is explained in [SOLUTION.md](SOLUTION.md) and  the challenge description can be found in [CHALLENGE.md](CHALLENGE.md)

## Prerequisites
Make sure you have installed all of the following prerequisites on your development machine:

* Node.js - [Download & Install Node.js](http://www.nodejs.org/download/) and the npm package manager.
You need to update npm to a 3.X.X version
To update to the latest npm version:

```bash
$ npm install -g npm
```

## Setup the application
Clone this repository, go to the coding-challenge-backend-c Express application folder and install the dependencies:

```bash
$ git clone hhttps://github.com/j-langlois/coding-challenge-backend-c
$ cd coding-challenge-backend-c
$ npm install
```

This command does a few things:

* First it will install the dependencies needed for the application to run.

* If you're running in a development environment, it will then also install development dependencies needed for testing and running the application.

You will also need a MongoDB cluster running on version 3.4 or above and setup up your connection by placing your uri inside `config/env.js`

```bash
process.env.MONGOLAB_URI = 'REPLACE_WITH_URI'
```


## Running the application
After the install process is over, you'll be able to run the application using NPM:

```bash
$ npm start
```
On your first run, you will need to seed your database by uncommenting the seed function inside `server.js`

The application should run on port 2345, so in your browser just go to [http://localhost:2345](http://localhost:2345).


## Before pushing
Before pushing anything on the repository, make sure that all tests are passing:

```bash
$ npm test
```

### Application structure

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
