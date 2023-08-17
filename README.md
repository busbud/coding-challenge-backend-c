# Busbud Coding Challenge

This repository contains a project developed as part of the [Busbud Coding Challenge](https://github.com/busbud/coding-challenge-backend-c). The project features an Express API written in TypeScript, with routing capabilities. The primary endpoint of this API is used for retrieving city suggestions based on user queries. The project employs caching using Redis for improved performance and queries data from a PostgreSQL database when necessary.

## Demo

You can access a live demo of the service at [jaskarn-busbud](https://jaskarn-busbud.onrender.com/). Please note that there might be a slight delay during the initial request due to a cold-start for free tier users of [Render](https://render.com).

To manually query the service, you can use the `/suggestions` route with various parameters.

## API Reference

#### Get results with a search term

```http
GET /suggestions?q=Toronto
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `q`       | `string` | **Required**. A city name. |

#### Get results with a search term and location

```http
GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
```

| Parameter   | Type     | Description                |
| :---------- | :------- | :------------------------- |
| `q`         | `string` | **Required**. A city name. |
| `latitude`  | `number` | **Required**. A latitude.  |
| `longitude` | `number` | **Required**. A longitude. |

Please note that `latitude` and `longitude` are required together. You cannot specify one without the other. Otherwise they are optional.

#### Examples

1. [Search Term](https://jaskarn-busbud.onrender.com/suggestions?q=Toronto)
2. [Search Term & Location](https://jaskarn-busbud.onrender.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163)

#### Raw Example

https://jaskarn-busbud.onrender.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

```bash
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

## Environment Variables

Ensure you set up the necessary environment variables by creating a `.env` file based on the provided `env.example.txt`.

## Prerequisites

1. Make sure you have [Docker](https://www.docker.com/products/docker-desktop/) installed.
2. Use [Node LTS](https://nodejs.org/en), which at the time of writing is `18.7.1`.

## Run Locally

Follow these steps to run the project locally:

#### 1. Clone the Project

Clone the project repository:

```bash
git clone https://github.com/jaskarnmankoo/coding-challenge-backend-c
cd coding-challenge-backend-c
```

#### 2. Install Dependencies

Navigate to the project directory and install the required dependencies:

```bash
cd coding-challenge-backend-c
npm ci
```

#### 3. Set Up Redis and PostgreSQL

Use Docker to set up Redis and PostgreSQL containers:

```bash
docker compose up
```

#### 4. Ensure pg_trgm extension is enabled in PostgreSQL

`psql` into your PostgreSQL:

```bash
psql "postgresql://admin:password123@127.0.0.1:6500/node_prisma"
```

Enable the `pg_trgm` extension and exit:

```sql
CREATE EXTENSION IF NOT EXISTS pg_trgm WITH SCHEMA pg_catalog;
\q
```

#### 5. Run Prisma Migrations and Seed (Optional)

If needed, run Prisma migrations to set up the database schema and seed initial data:

```bash
npx prisma migrate dev --name init
npm run seed
```

#### 6. Start the Server

Start the development server:

```bash
npm run dev
```

The API should now be up and running locally, and you can access it at `http://localhost:3000`.

## Running Tests

Ensure `Redis` and `PostgreSQL` are running via `Docker`, and then run the tests:

```bash
npm test
```

## Deployment

To deploy the project, choose a hosting provider like [Render](https://render.com) or any other of your choice. They generally provide [documentation](https://render.com/docs/web-services) for deploying `Node.js` applications.

For manual deployment, assuming you have the production `PostgreSQL` and `Redis` URLs, follow these steps:

#### 1) Build you application

```bash
npm run build
```

#### 2) Upload Your Build

Upload the `/dist` folder generated during the build process to your chosen hosting environment. This folder contains the compiled and optimized version of your application.

#### 3) Start you Application

Once the build is uploaded, start your `Node.js` application service. Run the following command to initiate your application:

```bash
npm start
```

# Design Decisions

This section outlines the key design decisions made during the development of this project, detailing the technologies, algorithms, and methodologies employed to ensure a robust and efficient system.

## Technologies Used

- **Node.js**: The backend of the application is built using Node.js, providing a non-blocking and event-driven architecture for optimal performance.

- **Prisma and PostgreSQL**: Prisma is utilized as the database toolkit, with PostgreSQL as the chosen database system. This combination ensures reliable data storage and efficient querying.

- **Redis**: Redis is employed for caching purposes, enhancing response times and reducing the load on the database.

- **GIN Index**: GIN (Generalized Inverted Index) is utilized to accelerate full-text searches, optimizing search performance and enabling faster retrieval of relevant data.

- **Jest and Supertest**: Jest is chosen as the testing framework, coupled with Supertest for API endpoint testing, guaranteeing the reliability and accuracy of the application's functionalities.

- **Typescript, ESLint, and Prettier**: Typescript is used to enhance code quality and maintainability. ESLint and Prettier enforce consistent code formatting and style, promoting a clean and standardized codebase.

- **npm Packages ([string-similarity-js](https://www.npmjs.com/package/string-similarity-js), [@turf/turf](https://www.npmjs.com/package/@turf/turf))**: The npm package 'string-similarity-js' is employed to implement the Sørensen–Dice coefficient algorithm for search term similarity scoring. Additionally, the '@turf/turf' package is utilized to calculate distances using the Haversine Formula when considering search terms along with latitude and longitude.

- **Docker**: Docker is utilized for local testing and development, ensuring a consistent environment across different stages of the application's lifecycle.

- **Render**: Render is chosen as the deployment platform, enabling seamless and reliable deployment of the application to a live environment.

## Search Algorithm

The decision to incorporate the Sørensen–Dice coefficient algorithm for search term similarity scoring ensures accurate and effective search result ranking. This algorithm provides a robust mechanism for evaluating the similarity between search terms and database entries.

Furthermore, the integration of the Haversine Formula, in conjunction with the Sørensen–Dice coefficient, enhances result scoring when geographic coordinates are provided. This allows the application to prioritize search results based on both textual relevance and geographical proximity.

## Rate Limiting

To maintain the stability and responsiveness of the application, a rate limiting mechanism has been implemented. This restricts the API to a maximum of 100 calls every 15 minutes, preventing excessive traffic and potential overload.

## Optimizations

An optimization that has been identified but not yet implemented is the integration of a pagination mechanism within the application's search results. Pagination provides a structured approach to displaying large sets of search results, enhancing user experience and performance.

# Acknowledgements

1. [docker-compose.yaml](https://codevoweb.com/api-node-typescript-prisma-postgresql-project-setup/#google_vignette) Used for quick local Redis setup.
2. [GIN Index](https://www.prisma.io/docs/concepts/components/prisma-schema/indexes) Prisma documentation on indexes.
3. [Scott Moss](https://frontendmasters.com/courses/api-design-nodejs-v4/) API design knowledge.
4. [Brian Holt](https://frontendmasters.com/courses/sql/) Indexing insights.
