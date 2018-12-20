# Busbud Coding Challenge

## General

The live version of the project is available on Heroku at: [https://thawing-bayou-30516.herokuapp.com](https://thawing-bayou-30516.herokuapp.com).

Example of search feature results: [/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163](https://thawing-bayou-30516.herokuapp.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163)


## About
### Cities import
The cities data from GeoNames are imported into a PostgreSQL database which works well with Heroku. On each release deployment
the ``` npm run import-cities ``` command is called by Heroku.

### Score
The score is calculated in two steps:
- A score relative to the query name (ex: ```?q=Londo```). The Levensthein Distance is used
for measuring the difference between the query name and each city name. 
- A score relative to the coordinates computed according to the distance between the requested location and each city location, 
relatively to a geo radius. This score can boost the query name score.

Each score is taken into account in the global calculation of the score. And each score has its weight in the global calculation if is filled
(ex: the maximum score for geo distance is 0.4 of 1 and 0.6 of 1 for the query name. But if no geo coordinate is filled, 
the query name score can reach the maximum (1), considering the relevance of the name only). 

### Suggestions endpoint call
A cache is created on each URL call, available for 5 minutes. This a memory store, but for a real app,
it would be better to use a production-ready cache client.

## Getting Started

Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and
[Windows](http://windows.github.com/) that make this easier.

### Setting up a Nodejs environment

Get started by installing [nodejs](http://www.nodejs.org).

For OS X users, use [Homebrew](http://brew.sh) and `brew install nvm`

Once that's done, from the project directory, run

```
nvm use
```

### Setting up the project

In the project directory run

```
npm install
```

### Create a database

Creat a PostgreSQL database.

```
createdb challenge
```

### Create an env file

Create an .env file corresponding to the values ​​of the .env.example.

```
cp .env.example .env
```

### Import the cities

Import the cities into the new created database.

```
npm run import-cities
```

### Running the tests

The test suite can be run with

```
npm test
```

### Running the linter

The linter can be run with

```
npm run lint
```

### Starting the application

To start a local server run

```
npm start
```

which should produce output similar to

```
Server running at http://127.0.0.1:4000/suggestions
```
