# Busbud Coding Challenge

## Requirements

Design an API endpoint that provides autocomplete suggestions for large cities.

The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

- [x] the endpoint is exposed at `/suggestions`

- [x] the partial (or complete) search term is passed as a query string parameter `q`

- [x] the caller's location can optionally be supplied via query string parameters `latitude` and `longitude` to help improve relative scores

- [x] the endpoint returns a JSON response with an array of scored suggested matches

- [x] the suggestions are sorted by descending score

- [x] each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)

- [x] each suggestion has a name which can be used to disambiguate between similarly named locations

- [x] each suggestion has a latitude and longitude

- [x] all functional tests should pass (additional tests may be implemented as necessary).

- [x] the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).

- feel free to add more features if you like!

# Solution

Challenge was written in Typescript (fully typed out) using Express. Suggestion speed is extremely fast and has rate limiting added to the `suggestions` endpoint.

#### Speed results

![](https://i.imgur.com/cY1HT8v.png)

#### Test Coverage

![](https://imgur.com/zyWpT6z.png)

#### Added Features

- Includes ESLint following standard TypeScript definitions.
- Api versioning. This is great/all most must have when working with apps. (to support old app versions)
- Great test coverage, added unit and integration tests as well. Updated the provided `suggestions.test.ts` with more tests.

### How to use the application

To use the application you will need to make a GET request to the endpoint `/v1/suggestions` or `/suggestions`. With the following parameters listed below:

|    Param    | Required |       Type       |                                         Description                                         |
| :---------: | :------: | :--------------: | :-----------------------------------------------------------------------------------------: |
|     `q`     |    \*    |      string      |                This will be the city name, can be a partial or complete name                |
| `latitude`  |          | string \| number | Used to get a better confidence score. **Note: Longitude is required when latitude passed** |
| `longitude` |          | string \| number | Used to get a better confidence score. **Note: Latitude is required when longitude passed** |
