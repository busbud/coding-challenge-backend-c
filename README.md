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

```
GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
```

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

```
GET /suggestions?q=SomeRandomCityInTheMiddleOfNowhere
```

```json
{
  "suggestions": []
}
```

### Non-functional

- All code should be written in Javascript or Typescript.
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
- `Docker` and `Docker Compose` (optional, recommended: v2.0.0 or higher)
- `VSCode` (optional source-code editor)

### Setting up your environment

1. Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and [Windows](http://windows.github.com/) that make this easier.

2. Install [nvm](https://github.com/nvm-sh/nvm#install--update-script) or your preferred node version manager.

3. Install [Node.js](http://www.nodejs.org).

### Setting up the project

> **Note**:
>
> In order to improve the development experience with vscode, we can add some debugging settings in order to debug locally and with docker.
>
> To launch the configurations follow the instructions detailed in the [official documentation](https://code.visualstudio.com/docs/editor/debugging#_launch-configurations).

Once the configuration file has been launched, replace its content with the following settings:

```json
{
  "version": "0.2.0",
  "configurations": [
    // Debug the app within docker.
    {
      "type": "node",
      "request": "attach",
      "name": "Attach to Docker",
      "protocol": "auto",
      "port": 5858,
      "restart": true,
      "localRoot": "${workspaceFolder}/",
      "remoteRoot": "/app"
    },
    // Debug the app locally.
    {
      "type": "node",
      "request": "launch",
      "name": "Local Nodemon",
      "protocol": "auto",
      "runtimeExecutable": "npm",
      "runtimeArgs": ["run", "debug"],
      "restart": true,
      "port": 5858,
      "console": "integratedTerminal",
      "envFile": "${workspaceFolder}/.env",
      "internalConsoleOptions": "neverOpen"
    }
  ]
}
```

### Working locally

In the project directory run:

```bash
nvm use
npm ci # To perform a clean install based on the defined packages.
```

### Running the tests

The test suite can be run with (integration and unit):

```bash
npm run test
```

To run only integration tests:

```bash
npm run test:integration
```

To run only unit tests:

```bash
npm run test:unit
```

To get the coverage report:

```bash
npm run cover
```

### Starting the application

To start a local server in development mode run:

```bash
npm run dev
```

it should produce an output similar to:

```bash
[nodemon] 2.0.6
[nodemon] to restart at any time, enter `rs`
[nodemon] watching path(s): *.*
[nodemon] watching extensions: js,mjs,json
[nodemon] starting `node ./src/app.js`
Server running at http://127.0.0.1:2345/suggestions
```

To start a local server in debugging mode run:

```bash
npm run debug
```

it should produce an output similar to:

```bash
[nodemon] 2.0.6
[nodemon] to restart at any time, enter `rs`
[nodemon] watching path(s): src/**/*
[nodemon] watching extensions: js,mjs,json
[nodemon] starting `node --inspect=0.0.0.0:5858 --nolazy ./src/app.js`
Debugger listening on ws://0.0.0.0:5858/b87538c9-6d95-43ec-a593-270f6d1d0ce0
For help, see: https://nodejs.org/en/docs/inspector
Server running at http://127.0.0.1:2345/suggestions
```

To start a local server in production mode run:

```bash
npm run start
```

it should produce an output similar to:

```bash
Server running at http://127.0.0.1:2345/suggestions
```

### Working with docker

> **Note**:
>
> The Node.js app runs on port 2345 by default, in case of using another port, the Dockerfile and docker-compose.yml files must be updated to export the new port respectively.

#### Use docker-compose to create the container in detach mode automatically:

```bash
docker-compose up -d
```

it should produce an output similar to:

```bash
Creating network "coding-challenge-backend-c_default" with the default driver
Building api
Step 1/8 : FROM node:12.18.4-alpine3.12
 ---> 1448646743d0
Step 2/8 : WORKDIR /app
 ---> Running in 1edcbb8a1d94
Removing intermediate container 1edcbb8a1d94
 ---> e21092080424
Step 3/8 : COPY package*.json ./
 ---> 249c8a9d754f
Step 4/8 : COPY . .
 ---> f54a072c9d9a
Step 5/8 : RUN npm ci
 ---> Running in ce26cf19d0b4

> nodemon@2.0.6 postinstall /app/node_modules/nodemon
> node bin/postinstall || exit 0

Love nodemon? You can now support the project via the open collective:
 > https://opencollective.com/nodemon/donate


> husky@4.3.0 install /app/node_modules/husky
> node husky install

husky > Setting up git hooks
Cannot read property 'toString' of null
husky > Failed to install

> husky@4.3.0 postinstall /app/node_modules/husky
> opencollective-postinstall || exit 0

Thank you for using husky!
If you rely on this package, please consider supporting our open collective:
> https://opencollective.com/husky/donate

added 541 packages in 9.176s
Removing intermediate container ce26cf19d0b4
 ---> 85c858f8865a
Step 6/8 : USER node
 ---> Running in bbc7f192aea0
Removing intermediate container bbc7f192aea0
 ---> 2820b2df1f9e
Step 7/8 : EXPOSE 2345
 ---> Running in 93a1386dcc3a
Removing intermediate container 93a1386dcc3a
 ---> 23be21bc2cfc
Step 8/8 : CMD [ "npm", "run", "debug" ]
 ---> Running in e8421cf79a9d
Removing intermediate container e8421cf79a9d
 ---> a0000a4e0f66

Successfully built a0000a4e0f66
Successfully tagged coding-challenge-backend-c_api:latest
WARNING: Image for service api was built because it did not already exist. To rebuild this image you must use `docker-compose build` or `docker-compose up --build`.
Creating coding-challenge-backend-c ... done
```

The -d option is for a detached mode allowing to run containers in the background, once the new containers has been created, it prints their names. Find out more at [docker's documentation site](https://docs.docker.com/compose/reference/up/).

#### For checking the container status just run:

```bash
docker-compose ps
```

it should produce an output similar to:

```bash
           Name                         Command               State                       Ports
--------------------------------------------------------------------------------------------------------------------
coding-challenge-backend-c   docker-entrypoint.sh npm r ...   Up      0.0.0.0:2345->2345/tcp, 0.0.0.0:5858->5858/tcp
```

#### For more details about the containers status run:

```bash
docker ps -a
```

it should produce an output similar to:

```bash
CONTAINER ID        IMAGE                            COMMAND                  CREATED             STATUS              PORTS                                            NAMES
98363d5592b5        coding-challenge-backend-c_api   "docker-entrypoint.s…"   3 minutes ago       Up 3 minutes        0.0.0.0:2345->2345/tcp, 0.0.0.0:5858->5858/tcp   coding-challenge-backend-c
```

### Additional resources from docker:

#### For accessing to any container run:

```bash
docker exec -it <CONTAINER> <SHELL>
```

#### To view the logs of Docker containers in real-time run:

```bash
docker logs -f <CONTAINER_ID>|<CONTAINER_NAME>
```

The `-f` or `--follow` option will show live log output.

#### To run tests within the docker container:

```bash
docker exec -it <CONTAINER> npm run test:unit
```

it should produce an output similar to:

```bash
> coding-challenge-backend-c@0.0.0 test:unit /app
> ./node_modules/mocha/bin/mocha --recursive test/unit/**/*.js



  Test the miscellaneous helper functions
    Test the filterCities function
      ✓ Must return a list of filtered cities
      ✓ Must return an empty array


  2 passing (18ms)
```

---

## Load testing

To perform some load testing we can use [artillery.io](https://artillery.io) which is really good and stright forward to use.

#### Examples:

To run a quick test for 60 number of arrivals with 20 number of requests for each arrival:

```bash
artillery quick -c 60 -n 20 "http://localhost:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163"
```

it should produce an output similar to:

```bash
Started phase 0, duration: 2s @ 10:29:42(-0300) 2020-10-28
Report @ 10:29:52(-0300) 2020-10-28
Elapsed time: 10 seconds
  Scenarios launched:  60
  Scenarios completed: 0
  Requests completed:  97
  Mean response/sec: 15.81
  Response time (msec):
    min: 557.8
    max: 4776.7
    median: 4473.8
    p95: 4766
    p99: 4775.7
  Codes:
    200: 97

Report @ 10:30:02(-0300) 2020-10-28
Elapsed time: 20 seconds
  Scenarios launched:  0
  Scenarios completed: 0
  Requests completed:  150
  Mean response/sec: 15.08
  Response time (msec):
    min: 4159
    max: 4547
    median: 4312.6
    p95: 4487.2
    p99: 4542.9
  Codes:
    200: 150

Report @ 10:30:12(-0300) 2020-10-28
Elapsed time: 30 seconds
  Scenarios launched:  0
  Scenarios completed: 0
  Requests completed:  125
  Mean response/sec: 12.94
  Response time (msec):
    min: 4047.5
    max: 4991.7
    median: 4479.6
    p95: 4893.3
    p99: 4988
  Codes:
    200: 125

Report @ 10:30:22(-0300) 2020-10-28
Elapsed time: 40 seconds
  Scenarios launched:  0
  Scenarios completed: 0
  Requests completed:  148
  Mean response/sec: 14.8
  Response time (msec):
    min: 4064.4
    max: 4381.9
    median: 4160.1
    p95: 4362.9
    p99: 4379.6
  Codes:
    200: 148

Report @ 10:30:32(-0300) 2020-10-28
Elapsed time: 50 seconds
  Scenarios launched:  0
  Scenarios completed: 0
  Requests completed:  142
  Mean response/sec: 14.43
  Response time (msec):
    min: 4096.6
    max: 4604.3
    median: 4315
    p95: 4533.5
    p99: 4555.1
  Codes:
    200: 142

Report @ 10:30:42(-0300) 2020-10-28
Elapsed time: 1 minute, 0 seconds
  Scenarios launched:  0
  Scenarios completed: 0
  Requests completed:  122
  Mean response/sec: 12.34
  Response time (msec):
    min: 4337.6
    max: 5136.6
    median: 4561.5
    p95: 5097.5
    p99: 5120.2
  Codes:
    200: 122

Report @ 10:30:52(-0300) 2020-10-28
Elapsed time: 1 minute, 10 seconds
  Scenarios launched:  0
  Scenarios completed: 0
  Requests completed:  122
  Mean response/sec: 12.39
  Response time (msec):
    min: 4557.7
    max: 5122.7
    median: 4886.5
    p95: 5095.7
    p99: 5120.7
  Codes:
    200: 122

Report @ 10:31:02(-0300) 2020-10-28
Elapsed time: 1 minute, 20 seconds
  Scenarios launched:  0
  Scenarios completed: 0
  Requests completed:  133
  Mean response/sec: 13.31
  Response time (msec):
    min: 4066.2
    max: 4931.6
    median: 4287.8
    p95: 4726.7
    p99: 4884.3
  Codes:
    200: 133

Report @ 10:31:12(-0300) 2020-10-28
Elapsed time: 1 minute, 29 seconds
  Scenarios launched:  0
  Scenarios completed: 60
  Requests completed:  161
  Mean response/sec: 10.84
  Response time (msec):
    min: 1973.7
    max: 4924.5
    median: 4266.9
    p95: 4795.6
    p99: 4922.5
  Codes:
    200: 161

All virtual users finished
Summary report @ 10:31:12(-0300) 2020-10-28
  Scenarios launched:  60
  Scenarios completed: 60
  Requests completed:  1200
  Mean response/sec: 13.43
  Response time (msec):
    min: 557.8
    max: 5136.6
    median: 4357.4
    p95: 4936.9
    p99: 5097.3
  Scenario counts:
    0: 60 (100%)
  Codes:
    200: 1200
```

We can perform tests with additional settings through a `.yaml` file, e.g:

The `.yaml` file could be located anywhere in the file system, we can create it via the command line as follows:

```bash
touch <some-descriptive-name>.yaml
```

The content within the `.yaml` file might look like:

```bash
config:
  target: http://localhost:2345
  phases:
    - duration: 120
      arrivalRate: 10
      rampTo: 20
scenarios:
  - flow:
      - get:
          url: "/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163"
```

To run the load tests using the `.yaml` file we can run:

```bash
artillery run <the-file-name>.yaml
```

For further details we can take a look at the [official documentation](https://artillery.io/docs/guides/guides/test-script-reference.html#Overview).
