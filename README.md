# Busbud Coding Challenge

To see the final result, you can visit: http://busbud.lucasreta.com/suggestions

## Installation

### Prerequisites

You are going to need:

- Git
- Node.js
- MongoDB

### Setting up the project

Some environment variables are needed to run the project.
Their description can be found in the `.env.template` file.
These variables can be set in your local environment or in
an `.env` file at the repository's root, which is recommended.

To do so, run the following command in the project directory:

```
cp .env.template .env
```

After adding the required environment variables, run the following
commands (also in the project's root directory):

```
npm install
npm run migrate
```

These commands will install the required dependencies and import
the data from the csv file inside the `data` directory into the 
configured database.

### Running the tests

The test suite can be run with:

```
npm test
```

### Starting the application

To start a local server run:

```
npm start
```

it should produce an output similar to:

```
Server running at http://127.0.0.1:2345/suggestions
```

### Development

To work on the application, one can run the command:

```
npm run start:hot-reload
```

which will start the server using nodemon. This will allow
for the server to automatically restart upon detected file
changes, thereby freeing oneself from the hastle of having to press about
three extra keys each time you want to kill it and restart it.

## Implementation Details

### Score

The algorithm for score calculation varies depending on user input:

- On requests that provide no information about the users' location (no coordinates on querystring),
the score is calculated using the [Sørensen–Dice coefficient](https://en.wikipedia.org/wiki/S%C3%B8rensen%E2%80%93Dice_coefficient) 
provided by [string-similarity](https://www.npmjs.com/package/string-similarity).
- On requests that provide user coordinates, the result of the Sørensen–Dice coefficient makes up one
fourth of the score, while the other three fourths are based on proximity to the location.

### Caching

Database caching was added with cachegoose, so as to improve performance in stress scenarios.

### Server

Considering the scope as a single-endpoint API, it seemed premature to add a framework on top
of the existing server structure.

