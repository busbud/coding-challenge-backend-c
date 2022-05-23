# Busbud Challenge - Solution

Here is my solution to the Busbud Coding Challenge in Node.js.

Please have a look at the [documentation](https://mattsyms-busbud-challenge.herokuapp.com/doc).

The HTTP server is written in TypesSript using [Koa](https://koajs.com).

PostgreSQL is used as database engine. In-memory SQLite is used as replacement in test environment. Interoperability with SQL database engines is handled by [Knex](https://knexjs.org).

Redis is used as caching layer to improve suggestions performance and service resilience.

CI and deployment to Heroku are automated with a GitHub workflow.

The suggestion engine works this way :

- It normalizes the input query string using ascii and case folding
- It performs a search against the database for city names matching the query string
- It calculates a population score for each match based on a maximum city polulation on earth
- It calculates a distance score for each match based on a maximum city distance on earth
- It computes a final score for each match using a weighted average of the population and distance scores
- It returns a suggestions list sorted by descending score and limited to a few matches

## Development environment

The `docker-compose.yml` stack provides the PostgreSQL and Redis services, along with an `app` container pre-installed with Node.js, NPM, Git and Heroku CLI.

The preferred way to run the project is within the app container. The provided `.env.sample` works out of the box for this purpose.

The project can be run as well with Node.js installed locally, and connected to local or remote PostgreSQL / Redis services. Make sure to customize the `.env` file accordingly.

## Setup

Create the `.env` file :

```
cp .env.sample .env
```

Deploy stack :

```
docker compose up -d
```

## App development

Get app container bash :

```
docker compose exec app bash
```

Install dependencies :

```
npm install
```

Run migration :

```
npm run migration
```

Run seed :

```
npm run seed
```

Run app in dev mode :

```
npm run dev
```

Run linter :

```
npm run lint
```

Run tests :

```
npm run test
```

Run tests with coverage :

```
npm run test:coverage
```

## PostgreSQL service

URL : `postgres://127.0.0.1:<DOCKER_POSTGRES_PORT>`

Get command line tool :

```
docker compose exec postgres psql -U postgres
```

## Redis service

URL : `redis://127.0.0.1:<DOCKER_REDIS_PORT>`

Get command line tool :

```
docker compose exec redis redis-cli
```

## Heroku

Authenticate to Heroku using a `.netrc` file :

```
heroku_email=<heroku_email> && \
heroku_token=<heroku_token> && \
echo -e "machine api.heroku.com\n  login $heroku_email\n  password $heroku_token" > ~/.netrc && \
echo -e "machine git.heroku.com\n  login $heroku_email\n  password $heroku_token" >> ~/.netrc
```

Get config

```
heroku config -a <heroku_app>
```

Set git remote :

```
heroku git:remote -a <heroku_app>
```

Deploy app :

```
git push heroku <branch>:main
```

Get logs :

```
heroku logs -a <heroku_app>
```
