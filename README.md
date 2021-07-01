
# Busbud Coding Challenge
[![js-standard-style](https://img.shields.io/badge/code%20style-standard-brightgreen.svg?style=flat)](https://standardjs.com/)

## Technical choice

### Search engine
The service need to find cities by their name and geolocation.
In this challenge, the data are small (~1MB), in that case,  it's possible to load these data into memory and use library like [fuse.js](https://fusejs.io/) to create our own search engine but there is still the problem of geolocation (maybe use a quadtree to index the location).

In production, the data volume can be more important (imagine all cities of world), then I needed a database to store these data. I chose ElasticSearch as search engine which provides these features (search as you type, geolocation search).

### Cache
All application in production need cache to mitigate with level of traffic. In this challenge I used Redis but we can easily change to an other store ([memcached](https://memcached.org/) for example).

### Web framework
The application is a microservice, a low-scope framework is a good choice for it.
Express is the most used framework but not the best choice for multiple reasons mentioned in [this article](https://dev.to/romainlanz/why-you-should-drop-expressjs-in-2021-711). For a year, I use [Fastify](https://www.fastify.io/), this framework is maintained by members of the Node.js core team and supported by the [openJS foundation](https://openjsf.org/). I don't advertise this framework :stuck_out_tongue: but it offers an excellent developer experience and very interesting performance.

## Project structure
This project is [split by components](https://github.com/goldbergyoni/nodebestpractices/blob/master/sections/projectstructre/breakintcomponents.md). Here is the file structure :

    src
    ├── components
    │   ├── health
    │   │   ├── api
    │   │   │   ├── controllers
    │   │   │   ├── documentation
    │   │   │   └── routes
    │   └── suggestions
    │       ├── api
    │       │   ├── controllers
    │       │   ├── documentation
    │       │   └── routes
    │       └── searchEngine
    │           ├── formatter
    │           └── repository
    ├── config
    ├── errors
    ├── helpers
    ├── migrations
    ├── plugins
    └── services
    │   ├── cache
    │   ├── logger
    │   │   └── transports
    │   └── searchEngine
    ├── gracefulShutdown.js
    ├── boot.js
    ├── buildApp.js
    ├── buildServices.js
    ├── run.js

### components
This folder contains the business logic for each components of system. It is composed of :
- `api` : Contains the `routes`, `controllers`  and `documentation` (swagger)
- `searchEngine` : Contains the `repository` used to fetch data into the search engine and `formatter` to format the data in a nice format.

The components use the differents services (see below) and they are injected into these components.

### config
Contains the configurations of application and services.

### errors
Contains the classes of errors (like HTTP errors for example)

### helpers
Contains helpers functions used by the application.

### migrations
This folder is still in development (I 'm not at all satisfied with the way I manage migrations) and contains the database migration (search engine at the moment)

### plugins
Contains the fastify plugins (it's like middleware in Express.js)

### services
Contains the differents services used by the application :

- `cache` : the cache manager
- `searchEngine` : Wrapper of elastic search client
- `logger` : the logger than contains multiple transport strategies (stdout, rotating file, etc.). I use [Pino.js](https://github.com/pinojs/pino) for the logger because it provide very low overhead.

## Getting Started

### Run with Docker
To facilitate development, the whole application can be run in docker containers. You can also, develop without used docker but you need a Redis and a ElasticSearch instance.

First install [Docker](https://docs.docker.com/get-docker/) and type the following command :

    docker-compose up --build
When the application is ready, the following message appears :

    suggestions-service-dev | [2021-07-01 18:01:02.184 +0000] INFO (32 on ef079126a2ff): Server listening at http://0.0.0.0:2345

Go to [http://0.0.0.0:2345](http://0.0.0.0:2345) and enjoy !

#### Run tests
To run the test, type the following command :

    TARGET=test docker-compose up --build
### Configuration
All configuration are store into a dotenv file. There are two dotenv files :

- `.env.docker` : Used by Docker container
- `.env.example` : If you want to run the application outside a container, copy its content in a `.env` file

You can change the value of each environment variable if you want :
```
# APP
APP_NAME=''  									# The name of application (use in the swagger doc)
APP_DESCRIPTION=''  							# The description of application (use in the swagger doc)
  
APP_MIGRATION_ENABLED=true  					# If true, run the migration

# LOGGER
LOGGER_TRANSPORT_ROTATING_FILE_ENABLED=true  	# Enable the rotating file strategy for log
LOGGER_TRANSPORT_ROTATING_FILE_FILE_NAME=''  	# The name of log file (the file are stored in /logs)
LOGGER_TRANSPORT_ROTATING_FILE_SIZE='10M'  		# The max size of log file
LOGGER_TRANSPORT_ROTATING_FILE_INTERVAL='1d'  	# The interval of rotating file
LOGGER_TRANSPORT_ROTATING_FILE_COMPRESS='true'  # Enable the compression of file
LOGGER_TRANSPORT_ROTATING_FILE_LEVEL=''  		# The log level (see Pino documentation : https://github.com/pinojs/pino/blob/master/docs/api.md#logger)
 

LOGGER_TRANSPORT_STDOUT_ENABLED=true  			# Enable the stdout logger
LOGGER_TRANSPORT_STDOUT_PRETTY=true  			# Print in a pretty format
LOGGER_TRANSPORT_STDOUT_LEVEL='info'  			# The log level (see Pino documentation : https://github.com/pinojs/pino/blob/master/docs/api.md#logger)

# SERVER  
SERVER_HOST='0.0.0.0'  							# The server host (use 0.0.0.0 if you run the application into a container)
SERVER_PORT=2345  								# The server host (use 0.0.0.0 if you run the application into a container)
  
# CORS  (see https://github.com/fastify/fastify-cors#options)
SERVER_CORS_ORIGIN=true  
SERVER_CORS_ALLOWED_HEADERS=null  
SERVER_CORS_CREDENTIALS=false  
  
#SEARCH ENGINE  
SEARCH_ENGINE_NODES=''  				# The URL of the elastic search nodes
SEARCH_ENGINE_MAX_RETRIES=5  			# Max retries of request or connection
SEARCH_ENGINE_REQUEST_TIMEOUT=60000  	# The request timeout
SEARCH_ENGINE_AUTH_TYPE='none'  		# The type of auth (none, basic, or apiKey)
SEARCH_ENGINE_AUTH_USERNAME=''  		# The username for basic auth
SEARCH_ENGINE_AUTH_PASSWORD=''  		# The password for basic auth
SEARCH_ENGINE_AUTH_API_KEY=''  			# The key for api key auth
  
#CACHE MANAGER  			
CACHE_IS_ENABLED=false  				# Enable the cache
CACHE_HOST='' 							# The redis server host
CACHE_PORT=6379  						# The redis server port
CACHE_PASSWORD=null  					# The redis password
CACHE_DB_INDEX=0  						# The redis index database
CACHE_TTL=600							# The Time To Live of data in cache exprimed in seconds
```
All environment variable present in this file can be overridden by the variables in the user environment (`process.env`). It's useful when you must pass secret variable in a CI pipeline for exemple or you want to pass variable in a container (with the `-e` option)

## Documentation

### Swagger documentation

A swagger documentation is available for endpoint : [http://localhost:2345/documentation](http://localhost:2345/documentation)

## Metrics
A prometheus endpoint is available : [http://localhost:2345/metrics](http://localhost:2345/metrics).
There are some default metrics recommended by Prometheus [itself](https://prometheus.io/docs/instrumenting/writing_clientlibs/#standard-and-runtime-collectors), requests duration histogram and requests duration summary.

## Convention
### Coding style
This project use [Standard](https://standardjs.com/) coding style (I used the [airbnb](https://github.com/airbnb/javascript) style for a long time but since I worked on opensource projects I use Standard)

### Commit message
This project use the [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/). This convention dovetails with SemVer, by describing the features, fixes, and breaking changes made in commit messages.

### Git hooks
This project use git hooks (via the library [husky](https://typicode.github.io/husky/#/)) to check to :

- Check if the commit message respect the [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) specification
- Format the code and check for errors before a commit


## TODO

- [ ] Improve the migration script (use a database to store the applied migrations and provide rollback)
- [ ]  Add more unit and integration tests
- [ ]  If the service run in a microservice architecture, use [jaeger](https://www.jaegertracing.io/) or [zipkin](https://zipkin.io/) to trace the request
- [ ]  Add CI pipeline
