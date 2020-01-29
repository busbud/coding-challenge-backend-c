# Busbud Coding Challenge

An API endpoint that provide autocomplete suggestions for large cities.

## Getting Started

These instructions will help you to run this project on your local machine for development and testing purposes. 


### Prerequisites

Things you need to install the software and how to install them
- node.js
- nvm

### Installing

A step by step to get your development env running

Change your node version typing `nvm use`

```sh
$ nvm use

> Found '.nvmrc' with version <0.10.26>
> Now using node v0.10.26 (npm v1.4.3)
```

run the project typing `npm start` 

```sh
$ npm start

> coding-challenge-backend-c@0.0.0 start
> node app.js

Server running at http://127.0.0.1:2345/suggestions

```

You can check the [live demo here.](https://andre682-backend-challenge.herokuapp.com/suggestions?q=maple&latitude=45.5265657&longitude=-73.59595) 

## API Documentation

This documentation describes the API endpoint and it's features

| Endpoint | Description | Method |
| :---:|:---|:---|
| [/suggestion](#suggestion) | Returns suggestions ordered by relevance (score) based on its query parameters| `GET` |

### /Suggestion 

Query parameters

| Params | Description | Required | Default |
|:----|:----|:---|:---|
| `q` | __[String]__ Search query string parameter | `true` | `""`| 
| `latitude` | __[Decimal]__ Geo latitude used to improve relevance by proximity | `false` | `undefined`
| `longitude` | __[Decimal]__ Geo longitude used to improve relevance by proximity | `false` | `undefined`

Payload

| body | Description |
|:---|:--|
| name | __[String]__ City name composed by [[City], [State/Province], [Country]] |
| latitude | __[Decimal]__ City location latitude |
| longitude | __[Decimal]__ City location longitude |
| score | __[Decimal]__ Result score by relevance |

### Examples

**Request using only query text parameter**

```
/suggestions?q=londo
```

***Response***
```
 HTTP/1.1 200 OK
Content-Type: application/json
```

```json
{
  "suggestions": [
    {
      "name": "London, ON, Canada",
      "latitude": "42.98339",
      "longitude": "-81.23304",
      "score": 0.8
    },
    {
      "name": "London, KY, United States",
      "latitude": "37.12898",
      "longitude": "-84.08326",
      "score": 0.8
    },
    {
      "name": "London, OH, United States",
      "latitude": "39.88645",
      "longitude": "-83.44825",
      "score": 0.8
    },
    {
      "name": "Londontowne, MD, United States",
      "latitude": "38.93345",
      "longitude": "-76.54941",
      "score": 0.5
    },
    {
      "name": "Londonderry, NH, United States",
      "latitude": "42.86509",
      "longitude": "-71.37395",
      "score": 0.5
    },
    {
      "name": "New London, CT, United States",
      "latitude": "41.35565",
      "longitude": "-72.09952",
      "score": 0.2
    },
    {
      "name": "New London, WI, United States",
      "latitude": "44.39276",
      "longitude": "-88.73983",
      "score": 0.2
    }
  ]
}

 ```

--- 

***Request using query text and geolocation***

```
/suggestions?q=vancouver&latitude=45.5265657&longitude=-73.59595
```

***Response***
 ```
HTTP/1.1 200 OK
Content-Type: application/json
```

```json
{
  "suggestions": [
    {
      "name": "Vancouver, BC, Canada",
      "latitude": "49.24966",
      "longitude": "-123.11934",
      "score": 0.9
    },
    {
      "name": "Vancouver, WA, United States",
      "latitude": "45.63873",
      "longitude": "-122.66149",
      "score": 0.9
    },
    {
      "name": "North Vancouver, BC, Canada",
      "latitude": "49.31636",
      "longitude": "-123.06934",
      "score": 0.2
    },
    {
      "name": "West Vancouver, BC, Canada",
      "latitude": "49.36672",
      "longitude": "-123.16652",
      "score": 0.2
    }
  ]
}

 ```

---

**_Atention:_ Responses without any result will return an empty array and 404 status code.**

```
/suggestions?q=SomewhereOverTheRainbow
```

***Response***
 ```
HTTP/1.1 404 Not Found
Content-Type: application/json
```

```json
{
  "suggestions": []
}

 ```

## Running the tests

Test the application by running

```
npm run-script test
```

### Break down into tests


```
  GET /notFound
    ✓ returns a 404 

  GET /suggestions
    with empty query
      ✓ returns a 404 
      ✓ returns an empty array of suggestions 
    with a non-existent city
      ✓ returns a 404 
      ✓ returns an empty array of suggestions 
    with a two letters search
      ✓ returns a 404 
      ✓ returns an empty array of suggestions 
    with a valid city
      ✓ returns a 200 
      ✓ returns an array of suggestions 
      ✓ contains a match 
      Validate the shape of the data being returned
        ✓ contains latitudes and longitudes 
        ✓ contains numeric scores 

```
