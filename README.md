# Busbud Coding Challenge

Author: Svetlana Brind

## API Description

The API exposes `/suggestions` endpoint that provides autocomplete suggestions for large cities.

- Returned suggestions are restricted to cities in the USA and Canada with a population above 5000 people.
- The suggestions are generated based on the value of a query string parameter `q` which must be passed with the call.
- Returned suggestions include `name`, `latitude`, `longitude`, `score` and are sorted by descending score
- The `score` is a value between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)

#### Sample responses

**Near match**

    GET /suggestions?q=Montr&latitude=37.5207&longitude=-77.37831

```json
{
  "suggestions": [
    {
      "name": "Montrose, VA, US",
      "latitude": 37.5207,
      "longitude": -77.37831,
      "score": "1.0"
    },
    {
      "name": "Montréal-Ouest, PE, CA",
      "latitude": 45.45286,
      "longitude": -73.64918,
      "score": "0.7"
    },
    {
      "name": "Montréal, PE, CA",
      "latitude": 45.50884,
      "longitude": -73.58781,
      "score": "0.3"
    },
    {
      "name": "Montrose, CO, US",
      "latitude": 38.47832,
      "longitude": -107.87617,
      "score": "0.0"
    }
  ]
}
```

**No match**

    GET /suggestions?q=SomeRandomCityInTheMiddleOfNowhere

```json
{
  "suggestions": []
}
```

## Prerequisites

You are going to need:

- `Git`
- `PostreSQL`
- `Node.js`

## Setting up local environment

1. Install dependencies by running:

```
npm install
```

2. Create `.env` and add `DB_HOST`, `DB_USERNAME`, `DB_PASSWORD` according to your local information

3. Initialize a database by running:

```
npm run set_db
```

4. Run database migration and seed the data by running:

```
npm run configure_db
```

5. Starte the server by running:

```
npm start
```

6. You should see following logs in the terminal

```
Server is running on port {PORT}
Database connected...
```

Done!

## Running the tests

The test suite can be run with:

```
npm run test
```

## Heroku

The application is deployed to heroku
https://murmuring-cove-87356.herokuapp.com/suggestions?q=Mont
