# Busbud Coding Challenge

## Summary

This project was developed over the template provided for the [**Busbud Coding Challenge**](https://github.com/busbud/coding-challenge-backend-c). It features an Express REST API written in TypeScript. The endpoint gives preference for cached results on Redis and queries the data on PostgreSQL database otherwise. The endpoint, parameters and responses are all according to the challenge's description.

It was not built on top of any boilerplate, each choice was planned ahead and each package and feature was carefully embedded. It's always a good time to learn and practice.

## Getting started

### Installing dependencies

Run the command:

```
npm install
```

### Setting up the environment

Create a `.env` file based on the `.env.example` contained in this repository.

#### Database

- `DATABASE_URL`: Connection URL to a PostgreSQL database.

  Example:
  ```
  DATABASE_URL="postgresql://user:password@host:port/databasename"
  ```

#### Server

- `SERVER_URL`: Address the application will listen to.

  Default: `127.0.0.1`

- `SERVER_PORT`: Port the application will listen to.

  Default: `2345`

- `SERVER_TRUST_PROXY`: Amount of trusted proxies between the user and the application.

  Default: ` ` (none)

#### Cache (Redis)

- `CACHE_URL`: Connection URL to a Redis database.

  Example:
  ```
  CACHE_URL="redis://user:password@host:port"
  ```

- `CACHE_TIMEOUT`: Timeout when connecting to the Redis database.

  Default: `5000`

#### Cities

- `DISTANCE_SCORE_PERCENTAGE`: Percentage the distance between the client's current location and the suggested cities will influence the score.

  Default: `0.5`

  Range: `0` (0%) to `1` (100%)

- `MAX_DISTANCE_ADD_SCORE`: Maximum distance (in KM) it will increase the score.

  Default: `1000`

- `LARGE_CITIES_POPULATION`: Minimum population large cities have (used to filter suggested cities).

  Default: `5001`

- `ACCEPTED_COUNTRY_CODES`: Accepted country codes (used to filter suggested cities).

  Default: `CA,US`

- `LIMIT`: Limit of suggested cities returned from the API's endpoint.

  Default: `10`

### Running the tests

If you're running the server for the first time, run the following command before running the tests to get the database properly initiated:

```
npm run init
```

If the `init` command was already executed, you can proceed to run the tests:

```
npm run test
```

### Starting a local server

To quickly start a local server, just execute the following command:

```
npm run dev
```

It's not necessary to execute the `init` command, as it's already executed during the `dev` and `build` commands.

## API description

These improvements were planned thinking on a real world scenario, addressing some main goals:

### Maintainability

It's really important to build a good API, but keeping it easy to maintain is very important as well. This is why I opted to implement **TypeScript** with strict options, making it safer when changing any code. Also brought **ESLint** with multiple options to create and keep standards, and made sure to have a clean structure and code.

Implemented **Jest** instead of Mocha, kept the existing tests and implemented some new ones as well. But this is an area of improvement - many other tests could've been implemented, specially for load testing.

**Swagger** was also used in order to provide an endpoint where any user could check the endpoints, it's parameters and responses.

### Powerful tools

Opted for **Prisma**, a relatively new and powerful ORM, bringing good performance and easiness at the migrations control. It features lots of options when building selects, updates and inserts, although just implemented raw queries in this project. **PostgreSQL** was the option for the database and **Redis** for the cache used on the endpoint.

### Performance

When talking about user's demand and requirements for good response times, we need to make sure our API runs as fast as possible while costing as little as possible. Thinking on a real world scenario, I execute the search on a PostgreSQL database taking into account all required filters (terms, population, countries) and retrieve only the requested specific amount. These results are saved in cache on **Redis**, so any other request using the same terms would be making use of the cache instead of the **PostgreSQL** database. The expire time of the cache is only 60 seconds and it's not renewed in order of making it easier to check during the tests.

Also paid attention to mitigate high levels of traffic by making use of the Express' Rate Limit middleware and handled trusted proxies.

### Accuracy

When studying a good and fast way of searching for the cities' names with the search terms, the **Trigraph** index search was a great option - being able to find partial matches, even if the user misstypes some letters. It also measures the similarity, which is used to calculate the city's score when featuring the list of suggested cities. As proposed at the challenge, the latitude and longitude, when supplied, are also taken into account for the score.

## Score calculation

The score consists on the sum of:

- the terms similarity to the suggested city's name;
- the distance between the supplied latitude and longitude, and the suggested city's latitude and longitude.

The similarity is calculated using the PostgreSQL's Trigraph index search.
The distance is calculated using the Haversine formula (https://en.wikipedia.org/wiki/Haversine_formula).

```
score = (similarity * similarity weight) + (distance score * distance score weight)
```

The distance score is not taken into account when the latitude and longitude is not provided. The score will then only consist of the terms similarity.