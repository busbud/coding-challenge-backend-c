# About the Solution

Main technical goal of the solution is to keep it simple and portable.

# Requirements
- Postgresql with fuzzystrmatch extension
- Composer
- PHP

## Setting up locally

```
git clone https://github.com/github-username/coding-challenge-backend-c.git
```

You can evaluate the solution by running app.sh in linux systems. For other
systems you should adapt the commands to start built-in php server on
port 8000 and with app.php as the router script.

For local evaluation a local database which can be connected by using following
uri is necessary.
```
postgres://php_challenge:php_challenge@localhost:5432/php_challenge
```
Also  fuzzystrmatch extension must be setup inside php_challenge database. This
can be achieved from psql client.
```
\connect php_challenge
CREATE EXTENSION fuzzystrmatch;
```

Init folder contains copydata.php which acts as a data migrator script. You can
migrate data using this php command.
```
php -f init/copydata.php
```
When operation completed you should see "Data copied successfully." message.

# Installation

- Setup a heroku account
- Install heroku client.
- Create a heroku instance.

```
heroku login
heroku create
```
Replace placeholder app name "heroku-app-name" with the instance name supplied from
heroku in this step.

## Setting up Postgresql on heroku
Documentation can be found at https://devcenter.heroku.com/articles/heroku-postgresql
Hobby setup is sufficient for this solution.

```
heroku addons:create heroku-postgresql:hobby-dev --version=11
```

### Setting up fuzzystrmatch for levenshtein
Solution uses levenshtein distance for the scoring algorithm.
Extension must be created by the instance root.
IMPORTANT : psql client must be installed on your local system.

```
heroku pg:psql --app heroku-app-name.git
CREATE EXTENSION fuzzystrmatch;
```
## Setting up repository
- Clone repository
- Add heroku as a remote
- Push php-challenge branch onto the main branch of heroku instance

```
git clone https://github.com/github-username/coding-challenge-backend-c.git
git remote add heroku https://git.heroku.com/heroku-app-name.git
git push -f heroku php-challenge:main
```

### Data migration
Composer should handle this on the first installation but if not, you can do
migration from heroku client by using bash.

```
heroku run bash -a heroku-app-name
php -f init/copydata.php
```
When operation completed you should see "Data copied successfully." message.

# Testing
Tests are ported from suggestions.js file. To run tests use this command.

```
vendor/bin/kahlan --reporter=verbose --spec=test
```

# Technical Notes

PDO is not handling numeric types in postgresql as it should be, so pgsql php
extension is preferred. pgsql.php contains basic connection mechanism which is
shared by copydata.php and suggestions.php.

Kahlan is chosen after an unsuccessful try with Peridot. Although missing some
methods, transferring tests was a smooth experience in Kahlan.

# Author

Mehmet Durgel <devfordever@gmail.com>
