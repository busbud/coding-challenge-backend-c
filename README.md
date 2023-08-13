# Busbud Backend Challenge

See the live site at https://busbud-gcp.onrender.com/suggestions?q=Montreal

It may take a minute or two to respond if the site hasn't been accessed recently.

Manually add query parameters to the link for latitude/longitude.

-   e.g. https://busbud-gcp.onrender.com/suggestions?q=Lon&latitude=43.70011&longitude=-79.4163

### Tech used

This TypeScript Express server (hosted on Render) uses Prisma to send SQL queries. A Postgres database responds from a Docker container hosted on Google Cloud Platforms.

---

## To get started, run Docker locally

You'll need Node v20.4.0, TypeScript and Docker (verify you have working `npm`, `tsc`, and `docker` commands).

In repo directory:

-   `nvm use` and `npm install`
-   Ensure you have the `.env` file in root directory
-   `npm run build` starts pulling the image and drawing up a containerized DB **_and_** server
-   Upon seeing `Listening on port 4000`, open a new terminal for `npm run db-init`
-   After a moment, the server should be listening and the seed command should have executed
-   Test at http://localhost:4000/suggestions?q=Mont

Note that subsequent docker compose up and down commands (from an `npm run xyz`) might create errors from the Postgres terminal about duplicate keys. At that point, just use `npm run server`.

This local repo has been tested on 3 machines, Pop!_OS 22.04 and two different Windows 10 installations. All worked without issue.

An **M1 Mac** came across the following error and was unable to seed the DB: https://github.com/prisma/prisma/issues/18207. I don't actually own the M1 Mac and so can't offer much help on troubleshooting it.

---

## Testing

To call the entire test suite, `npm run test` while the `Listening on port 4000` terminal is active.

Tests were added to:

-   Verify SQL injection doesn't work
-   Ensure latitude/longitude values are between -90 and 90
-   Show that coordinates provide a more confident autocomplete suggestion
-   Confirm that a 429 Too Many Requests error code can occur

---

# Architecture Decisions

## Why Prisma?

Prisma makes it really easy to both migrate schemas and seed databases. Prisma can integrate with various SQL databases, can be used as a query builder, and helps enforce client side type safety.

$queryRaw is a way to manually write SQL queries for when the query builder isn't enough. It allows for conditional rendering of SQL commands as well as template string interpolation, see /src/findCities.ts lines 33 and 49.

"**... but can't that lead to SQL injection??**"

Nope!

```
const query = '; DROP TABLES "City";--'

const result = await prisma.$queryRaw`${query}`
```

Prisma plans for that by making any interpolated variables (i.e. our `const query`) **only** be processed as data and never as SQL keywords. This means that `const result` will create a syntax error. It reads the SQL query as just a string, no keywords. Without any keywords, nothing can be deleted or modified. The database is safe.

### Key Ideas

-   The Schema.prisma file contains the definition of the database schema (in Prisma called a model) which will be used to populate the database with the correct table fields and types, resulting in a very easy TypeScript integration
-   /Prisma/seed.ts enjoys a very simple prisma.city.create query builder that seeds the database from the parsed TSV file
-   SQL injection is taken care of

---

## Why Docker?

Containerization perhaps feels like the _big new thing_ in development and for good reason. I wanted to make the experience of any potential consumer of this repo seamless, and Docker is the best way to do that. No need to install Postgres or pgAdmin, just have Docker ready and run a couple commands and Bob's yer uncle.

### Key Ideas

-   Docker runs on _images_: a version of programs like NodeJS, Alpine (lightweight Linux), Python, etc., any runtime environment one might need in order to develop code
-   The Google Cloud VM is just running Postgres. It was seeded remotely and the data persists in a Docker volume
-   The local Docker container spins up both Postgres and a NodeJS server. Dockerfile and Docker Compose are essential files providing important information for how each _service_ should be run. e.g. What images to use, what .env variables, any commands/scripts to run upon initialization, etc

---

## Why Google Cloud Platform?

I wanted a free option just for proof of concept and I knew this option would be free by choosing low enough specs. I felt like Google would be a reliable choice and would potentially be useful for future projects at a proper production scale.

GCP let me open up an SSH terminal on a virtual Debian machine (VM) in the browser and then install Docker via the command line. I then spun up a simple Postgres image. From my Linux machine, I made a VM running Linux that itself was running Linux in Docker. It's Linuxes all the way down!

The live site may have taken a minute or so to respond. _"Cold starts"_ like this occur when the VM instance gets recycled after inactivity. In other words, the virtual machine, alive and ready to process requests on Google's servers, eventually just gets turned off if it receives no more requests. Once it receives a request, it has to turn on. Imagine the time it takes for a computer to boot up and load its first program and you can appreciate why it might take a solid minute for that first request to come back with a result.

### Key Ideas

-   A virtual machine is a bit like Docker, where you can run an operating system as if it were just another program, like running Firefox or Photoshop
-   My VM is running Debian (a Linux OS) within which runs Docker, which is then running Postgres, providing data back to my NodeJS server
-   Note that the _matryoshka-doll-ification_ of programs isn't a necessary component, it's just fun and enables some workarounds

---

# Further Research

-   Multiple containers to _'horizontally scale'_ and thereby mitigate high levels of traffic. Each instance of a database/server could individually handle requests, so the load is more evenly distributed.

-   This likely requires a front-end, but putting both the Postgres and NodeJS app within the same Docker container (like we're doing locally) in the VM would've been something cool to try. Barring performance issues from the free tier, this could be an easy solution to hosting, meaning one less web service to set up