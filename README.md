# Busbud Coding Challenge - Murilo Campaner
Code challenge implemented for Busbud during the hiring process

This API provides autocomplete suggestions for large cities.
The results are restricted to the cities of the **United States** and **Canada**, with a population greater than **5000** peoples.

## Development

### Prerequisites
You are going to need:

- `Git`
- `nvm` (or your preferred node version manager)
- `Node.js > 16.*`
- `Docker` (for running Elasticsearch and Kibana locally)

### Installation Instructions
### 1. Installation

Clone the project and install the dependencies with following commands:

```bash
# Clone the project
git clone https://github.com/luxurypresence/geocode-api.git

# Open folder
cd geocode-api

# Install dependencies
npm install
```

Copy the `.env.sample` file to `.env` and update the environment variables if necessary.

```bash
APP_PORT=8000
ELASTIC_SEARCH_URL=http://0.0.0.0:9200
```

#### 2. Start Kibana and Elasticsearch using `docker-compose.yml` <small>(only for development)</small>
```bash
docker-compose up
```


#### 3. &nbsp;&nbsp;Create Elasticsearch indexes
```bash
yarn seed
```

#### 4. Start the project
`yarn start` or `npm start`

## Endpoints
Id ipsum officia ut elit amet et excepteur voluptate anim. Non adipisicing culpa tempor deserunt aliquip Lorem occaecat cupidatat nostrud dolor reprehenderit cupidatat. Nulla eiusmod in consequat id sunt dolor nostrud sunt aute fugiat incididunt amet. Fugiat non pariatur nostrud elit magna culpa qui cupidatat sint irure dolore aliqua cillum. Do nisi mollit officia ut dolor ex Lorem velit voluptate. Pariatur laborum cillum enim deserunt nostrud culpa nisi.

> [Download Postman Collection>](#)

### Autocomplete endpoint
#### `[GET] /suggestions`

With this endpoint you can fetch autocomplete suggestions by sending an search query to API

##### Query Params

| Name                   | Default |  Type  |              Example               |
| :--------------------- | :----: | :----: | :--------------------------------: |
| **q** <small>*(required)*</small> | - |string  |  1109 N Highland St, Arlington VA  |
| **latitude** <small>*(optional)*</small>       | - | number   | `42.98339` |
| **longitude** <small>*(optional)*</small>      | - | number   | `-81.23304` |
| **limit** <small>*(optional)*</small>      | 100 | number   | `100` |
| **offset** <small>*(optional)*</small>      | 0 | number  | `0` |

##### Request example:

```
http://<URL>/suggestions?q=searchTerm&latitude=38.886672&longitude=-77.094735&limit=100&offset=0
```

##### Response example

##### HTTP 200
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

##### HTTP 404
```json
{
  "suggestions": []
}
```
---


### Debuging Queries with Kibana
After starting the project, access Kibana on your browser at `http://localhost:5601`

### Tests

At the project folder, run `yarn test` or `npm run test` in terminal
___

### Deployment - Heroku

Do anim amet aute nulla veniam officia non nisi sit voluptate do excepteur. Incididunt enim non velit consequat veniam nulla. Ullamco elit minim nisi culpa cupidatat ut culpa tempor irure fugiat excepteur. Sunt ipsum amet fugiat id Lorem nisi ex. Ut nisi nisi exercitation ullamco aliquip ipsum fugiat aute in velit esse laboris sint exercitation. Aliquip Lorem eu do veniam magna fugiat occaecat do. Quis do dolore ipsum Lorem consequat occaecat aute ullamco exercitation aliqua pariatur.

___


### Features
- [x] Limit & Offset Params
- [x] Batch queue to Elasticsearch
- [x] Docker for Elasticsearch + Kibana
- [x] .env structure
- [x] Seed script (`es-seed.ts`)
- [x] Typescript configuration
- [ ] Mitigations to handle high levels of traffic should be implemented.
- [ ] Deploy to Heroku