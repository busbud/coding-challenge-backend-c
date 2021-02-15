## Busbud Challenge by Bünyamin AKÇAY<bunyamin@bunyam.in>

This application based on PHP and Lumen Framework. For more information visit  [Lumen framework official website](https://lumen.laravel.com/)

Visit live demo on [Heroku](https://thawing-bayou-65299.herokuapp.com/)

Runs With Apache & Mysql and Filecache

_** Before installation:  lumen has prefered for stability, laravel trust with lightweight development equipment. Under these conditions Pure-PHP might be chosen but being modern software development standarts would not match._

Lumen Minimum System Requirements below

```
PHP >= 7.3
OpenSSL PHP Extension
PDO PHP Extension
Mbstring PHP Extension
```

Before using this application follow instructions below

_For mysql importing PhpMyAdmin is highly recommended_ 

## 1- MYSQL database handling

Cities database is included main directory of this repo

Mainly required columns are only converted tsv to mysql format, for more rapid query responses mysql index are included.

Before importing you need to have a database, Create new database from PhpMyadmin  or Run this command for


>CREATE DATABASE newdatabase1


## 2- MYSQL importing

Download 'cities_canada_usa.sql' file from this repo and  import with phpmyadmin or via mysql cli


>mysql -u mysqluser -pmysqlpassword newdatabase1 < cities_canada_usa.sql


## 3- Env File
_if you use heroku every single env file parameter should import by config , env files ignoring in heroku apps_

rename .env.example file to .env and fill parameters for database.

```ssh
DB_HOST=host_or_ip_mysql_server
DB_PORT=mysql_port_default_3306
DB_DATABASE=database_name
DB_USERNAME=attached_mysql_user
DB_PASSWORD=attached_mysql_user_password
```

## 4- Composer
use this command below , if composer not installed your enviroment visit [getcomposer.org](https://getcomposer.org)



>composer install


## 5- Run

with command below built-in php server will serve in port 8000

>php -S localhost:8000 -t public 




##Test by browser

you can check by visit with browser in [localhost:8000](http://localhost:8000)

##Test by Get request
You can use any agent or query builder for getting request such as postman
```request
Request:
Host: thawing-bayou-65299.herokuapp.com
Method: GET
Headers:
X-Busbud-Token: PARTNER_BaASYYHxTxuOINEOMWq5GA
Query parameters:
q:search_term
```
>please note  suggestion api does not work without 'X-Busbud-Token'




##Unit Testing
Unit test will make correct request is valid or not, If unit test not pass database connection or minimum requirement may not matched.

To see  testcases use command below


>php vendor/phpunit/phpunit/phpunit  --list-tests 


To apply testcases use command below


>php vendor/phpunit/phpunit/phpunit 


##Technical 
* Responses not directly queried on mysql, hits cache for query parameter and location parameters if defined.
* Currently using file cache due tu minimum system requirements, memcache or redis may use for caching.
* Browser may ask user's location to add location parameters to suggestion endpoint
* On MySql Search case results are limited over 50K population field and wildcard search on name field

---

# Busbud Coding Challenge

## Requirements

Design an API endpoint that provides autocomplete suggestions for large cities.
The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

- the endpoint is exposed at `/suggestions`
- the partial (or complete) search term is passed as a query string parameter `q`
- the caller's location can optionally be supplied via query string parameters `latitude` and `longitude` to help improve relative scores
- the endpoint returns a JSON response with an array of scored suggested matches
    - the suggestions are sorted by descending score
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    - each suggestion has a name which can be used to disambiguate between similarly named locations
    - each suggestion has a latitude and longitude
- all functional tests should pass (additional tests may be implemented as necessary).
- the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).
- feel free to add more features if you like!

#### Sample responses

These responses are meant to provide guidance. The exact values can vary based on the data source and scoring algorithm.

**Near match**

    GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

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

**No match**

    GET /suggestions?q=SomeRandomCityInTheMiddleOfNowhere

```json
{
  "suggestions": []
}
```


### Non-functional

- All code should be written in Javascript, Typescript or PHP.
- Mitigations to handle high levels of traffic should be implemented.
- Challenge is submitted as pull request against this repo ([fork it](https://help.github.com/articles/fork-a-repo/) and [create a pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)).
- Documentation and maintainability is a plus.

## Dataset

You can find the necessary dataset along with its description and documentation in the [`data`](data/) directory.

## Evaluation

We will use the following criteria to evaluate your solution:

- Capacity to follow instructions
- Developer Experience (how easy it is to run your solution locally, how clear your documentation is, etc)
- Solution correctness
- Performance
- Tests (quality and coverage)
- Code style and cleanliness
- Attention to detail
- Ability to make sensible assumptions

It is ok to ask us questions!

We know that the time for this project is limited and it is hard to create a "perfect" solution, so we will consider that along with your experience when evaluating the submission.

## Getting Started

### Prerequisites

You are going to need:

- `Git`
- `nvm` (or your preferred node version manager)
- `Node.js`

### Setting up your environment

1. Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and
[Windows](http://windows.github.com/) that make this easier.

2. Install [nvm](https://github.com/nvm-sh/nvm#install--update-script) or your preferred node version manager.

3. Install [Node.js](http://www.nodejs.org).

### Setting up the project

In the project directory run:

```
nvm use
npm install
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
Server running at http://127.0.0.1:2345/suggestions
```
