City Suggestion Application
========================

This application is that offers users suggestions so that they can find the cities they are looking for more easily. 
This project used to Symfony Framework(5.2 version)

# Getting Started

## Prerequisites

You are going to need:

- `Git` [learn more information Git][1]
- `Composer` [learn more information Composer][2]
- `Symfony Framework` [learn more information Symfony][3]

Requirements
------------

* PHP 7.2.5 or higher;
* PDO-SQLite PHP extension enabled;
* ctype, iconv extensions installed;

Installation
------------

Download the repository and run this command for php packages install:

```bash
$ cd project_directory/
$ composer install
```

Run Project
-----

There's need to symfony-cli to run the application. If you don't have symfony-cli please follow [Symfony Download][4]

Then you need to run these commands:

```bash
$ cd project_directory/
$ symfony server:start
```
You will probably see output like: [OK] Web server listening http://127.0.0.1:8080 Project Run!

Usage
-----
You can use in your local:
```bash
GET http://127.0.0.1:7071/suggestions/?q=londo

With Coordinate:

GET http://127.0.0.1:7071/suggestions/?q=londo&latitude=38.88645&longitude=-83.44825
```

OR You can use in heroku:
```bash
GET https://bus-city-suggestion.herokuapp.com/suggestions/?q=londo

With Coordinate:

GET https://bus-city-suggestion.herokuapp.com/suggestions/?q=londo&latitude=38.88645&longitude=-83.44825
```


You will probably see results like:
```bash
[
  {
    "name": "London, OH, US",
    "latitude": 39.88645,
    "longitude": -83.44825,
    "score": 1
  },
  {
    "name": "London, KY, US",
    "latitude": 37.12898,
    "longitude": -84.08326,
    "score": 0.8
  },
  {
    "name": "London, 8, CA",
    "latitude": 42.98339,
    "longitude": -81.23304,
    "score": 0.6
  },
  {
    "name": "Londontowne, MD, US",
    "latitude": 38.93345,
    "longitude": -76.54941,
    "score": 0.4
  },
  {
    "name": "New London, WI, US",
    "latitude": 44.39276,
    "longitude": -88.73983,
    "score": 0.2
  }
]
```

Tests
-----

Execute this command to run tests:

```bash
$ cd project_directory/
$ php bin/phpunit tests
```

[1]: https://git-scm.com/
[2]: https://getcomposer.org/
[3]: https://symfony.com/
[4]: https://symfony.com/download