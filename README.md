# Busbud Coding Challenge

## See it Live at 

https://busbud-challenge-edward.herokuapp.com/suggestions?q=


## Notes

Used NestJS and Prisma hosted on Heroku. 

Used pg_trgram, GIN and to_tsvector to vectorize, index search results with weights 

Used Jest for testing

## Requirements Met 
✅ - the endpoint is exposed at `/suggestions`
✅ - the partial (or complete) search term is passed as a query string parameter `q`
✅ - the caller's location can optionally be supplied via query string parameters `latitude` and `longitude` to help improve relative scores
✅ - the endpoint returns a JSON response with an array of scored suggested matches
    - the suggestions are sorted by descending score
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    - each suggestion has a name which can be used to disambiguate between similarly named locations
    - each suggestion has a latitude and longitude
✅ - all functional tests should pass (additional tests may be implemented as necessary).
✅ - the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).

✅ - All code should be written in Javascript, Typescript or PHP.
✅ - Mitigations to handle high levels of traffic should be implemented.


## Getting Started

### Prerequisites

You are going to need:

- `Git`
- `nvm` (or your preferred node version manager)
- `Node.js`
- `Node version 16 or above`

To run locally: 

- `yarn install or npm install`
- `npx prisma migrate dev - to generate pgsql table and queries as well as seed the database`
- `yarn run test - to run tests`
- `yarn run dev - to start development server`
- `http://localhost:${your port}/suggestions?q=` 
- `add any query params after =`
