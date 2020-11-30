# Busbud Coding Challenge
## Perquisites
- [Node.js](https://nodejs.org/en/)
- [Docker](https://www.docker.com/products/docker-desktop)
- [(Optional) Postman](https://www.postman.com/downloads/)
- NPM (comes bundled with Node.js)

This application has been built on MacOS Catalina, using Node.js v14.15.0, NPM 6.14.8, Docker 19.03.13

## Local Build Instructions
1. Clone repository: `git clone <repository-url>`
2. Navigate to folder: `cd <path-to-git-repository>`
3. Start docker containers: `docker-compose up --build`

At this point two docker containers should now have been started:
- coding-challenge-backend-c_app_1: A Node.js application serving an Express API on `localhost:3000`
- coding-challenge-backend-c_db_1: A PostgreSQL Database which may be connected to using the following connection string `postgres://user:pass@localhost:35432/db`. All database initialization scripts should be automatically executed on the first build of the docker container, should any errors occur please see the next section.

## Database Initialization Issues
If the database needs to be reconfigured, or is not correctly configured please see the options available below.

### 1. Deleted Containers and Re-build
1. List containers: `docker ps -a`
2. Delete coding-challenge-backend-c_db_1 container: `docker rm <container-id>`
3. List images: `docker images`
4. Delete coding-challenge-backend-c_db_1 image: `docker image rm <image-id>`
5. Re-build docker containers: `docker-compose up --build`

### 2. Manual Initialization
The database can be manually initialized by executing the four SQL scripts found in `db/`in numerical order in your favorite PostgreSQL editor.

The [PostgreSQL](https://marketplace.visualstudio.com/items?itemName=ms-ossdata.vscode-postgresql) Visual Studio Code is a great option to connect to Postgres DBs and execute SQL.

## Using the API
A Postman Collection has been provided in the [docs/](https://github.com/Shmeve/coding-challenge-backend-c/blob/develop/docs/Busbud.postman_collection.json) folder with pre-configured requests to:
- Local Development Environment: `http://localhost:3000/`
- Live Heroku Environment: `https://code-challenge-app.herokuapp.com/`
