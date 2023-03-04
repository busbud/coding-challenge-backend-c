# Runbook for Suggestions API 

## Local Development using docker

### For running the API

From the root folder, run either
```sh
docker-compose up coding-challenge
```

This should create a docker image named `coding-challenge-backend-c_coding-challenge` and start a container named `coding-challenge` with the following expected output

```sh
> coding-challenge-backend-c@0.0.0 dev
> NODE_ENV=development ./node_modules/nodemon/bin/nodemon.js --watch app -e ts --exec 'ts-node app/index.ts'

coding-challenge | 
coding-challenge | > coding-challenge-backend-c@0.0.0 dev /code
coding-challenge | > NODE_ENV=development ./node_modules/nodemon/bin/nodemon.js --watch app -e ts --exec 'ts-node app/index.ts'
coding-challenge | 
coding-challenge | [nodemon] 2.0.20
coding-challenge | [nodemon] to restart at any time, enter `rs`
coding-challenge | [nodemon] watching path(s): app/**/*
coding-challenge | [nodemon] watching extensions: ts
coding-challenge | [nodemon] starting `ts-node app/index.ts`
coding-challenge | {"level":30,"time":1677899443598,"pid":29,"hostname":"bf52949bb75e","name":"development","msg":"Suggestions server listening at http://[::]:8080","serverRunId":"74720a34-7b3e-4915-a446-b0cd1592c056","tags":["start"]}
```

The api can now be accessed at
```sh
http://0.0.0.0:8080/
```
OR at
```sh
http://localhost:8080/
```

### For running the unit and functional tests together using docker-compose
```sh
docker-compose up coding-challenge-test
```


## Local Development outside docker

### For running the API

From the root folder, run
```sh
npm i
```
```sh
npm run dev
```
Expected output
```
> coding-challenge-backend-c@0.0.0 dev
> NODE_ENV=development ./node_modules/nodemon/bin/nodemon.js --watch app -e ts --exec 'ts-node app/index.ts'

[nodemon] 2.0.20
[nodemon] to restart at any time, enter `rs`
[nodemon] watching path(s): app/**/*
[nodemon] watching extensions: ts
[nodemon] starting `ts-node app/index.ts`

{"level":30,"time":1677898904267,"pid":705267,"hostname":"localhost","name":"development","msg":"Suggestions server listening at http://[::]:8080","serverRunId":"f3cdd292-a499-4cad-a678-fae540d3d6ae","tags":["start"]}
```

The api can now be accessed at
```
http://0.0.0.0:8080/
```
OR at
```
http://localhost:8080/
```

### For running the unit and functional tests together
```sh
npm run test
```

### For linting the code
```sh
npm run lint
```
OR
```sh
npm run lint:fix
```

## Using your other instance of Redis
To use a different instance of Redis, just update the following variables in `.env` file at the root of the project

```
REDIS_HOST=
REDIS_PORT=
REDIS_PASSWORD=
```

## Using your other instance of Firestore
To use a different instance of Redis, just update the variables `.env` file at the root of the project
```
GOOGLE_APPLICATION_CREDENTIALS=
FIRESTORE_PROJECT_ID=
```
AND replace the keyfile named `googleApplicationCred.json` from your own keyfile
