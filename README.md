# Busbud Coding Challenge By Baila Kahn

This is an API to get city search suggestions based on client query and location

## API Documentation

This API has been made for the sake of simplicity and ease of running it locally without having to install too many dependencies.
Here are few details:

- API does not have authentication.
- MongoDB or ElasticSearch would have been better to query the data especially with geo data in play
- I acknowledge that using simple javascript array functions `filter`, `map` and `sort` is not best for performance especially with big data

## Endpoints

```
GET /suggestions
```

### REQUEST

#### Params

- `q`: [String]
- `latitude`: [String, Optional]
- `longitue`: [String, Optional]

### Response

```
200
```

- Attributes (Array[Suggestion])

#### Data structures

##### Suggestion

- `name`: [String] Formated city name
- `latitude`: [String], User location latitude
- `longitude`: [String], User location longitude
- `score`: [Number], Suggestion confidence

#### Example

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
      "score": 0.5
    },
    {
      "name": "London, KY, USA",
      "latitude": "37.12898",
      "longitude": "-84.08326",
      "score": 0.5
    },
    {
      "name": "Londontowne, MD, USA",
      "latitude": "38.93345",
      "longitude": "-76.54941",
      "score": 0.3
    }
  ]
}
```

```
5xx, 4xx
```

- Attributes ([Error])

#### Data structures

##### Error

- `type`: [String] Error Type
- `code`: [String], Error code
- `message`: [String], Error Message

#### Example

```json
{
  "type": "InternalServerError",
  "code": "errors.internal",
  "message": "baila is not defined"
}
```

## Getting Started

- This api uses path aliases to avoid the `../../../../../foler` hell
- ALiases can be found in package.json and are self explanatory
- This also allows to move folders arround without worring about import/require being broken

### Prerequisites

All dependencies have been updated to recent version.

- `Note` This change may require updates of the application core and tests

You are going to need:

- `Git`
- `nvm` (or your preferred node version manager)
- `Node.js v16.x`
- `NPM v7.x`

### Setting up the project

In the project directory run:

```
npm install 16.0.0
nvm use 16.0.0
npm install
npm run postinstall
```

### Endpoint Structure

```
endpoints
└───category
│   └───endpoint
│       │   index.js Endpoint entry file
│       │   config.json Endpoint configuration, Here you can specify API METHOD, Custom ROUTES etc...
│   └───────units
|       |       index.js all the endpoint functions are imported from here
|       |       function1.js an endpoint function
```

### Creating a new endpoint

New endpoints will be found under `endpoints/[category]`

```
YARN
yarn new:endpoint --p=[category]/[endpoint]

NPM
npm run -- new:endpoint --p=[category]/[endpoint]
```

### Creating a new unit/function

New units will be found under `endpoints/[category]/[endpoint]/[name]`

```
YARN
yarn new:unit --p=[category]/[endpoint]/[name]

NPM
npm run -- new:unit --p=[category]/[endpoint]/[name]
```

### Running the tests

The test suite can be run with:

```
npm run test
```

### Starting the application

To start a local server run:

```
npm run start
```

it should produce an output similar to:

```
Server running on port 2345!
```
