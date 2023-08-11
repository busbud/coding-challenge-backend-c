# Busbud Backend Challenge

See the live site at https://busbud-gcp.onrender.com/suggestions?q=Montreal

It may take a while to respond upon first load.

Manually add query parameters for latitude/longitude to the link.

-   e.g. https://busbud-gcp.onrender.com/suggestions?q=Lon&latitude=43.70011&longitude=-79.4163

### Tech used

This TypeScript Express server uses Prisma to send SQL queries. A Postgres database responds from a Docker container hosted on Google Cloud Platforms. The repo also contains instructions to run these services in a local Docker container.

---

## To get started, run Docker locally

You'll need Node v20.4.0 and Docker CLI.

In repo directory:

-   `npm install`
-   Ensure the URL for locally hosted Docker is present in `.env`
-   `npm run build` starts pulling the image and drawing up a containerized DB **_and_** server
-   Upon seeing `Listening on port 4000`, open a new terminal for `npm run db-init`
-   Server should be listening and the seed command should now have executed
-   Test at http://localhost:4000/suggestions?q=Mont

Note that subsequent compose up and down commands might create errors from the Postgres terminal about duplicate keys. At that point, just use `npm run server`.

---

## Testing

To call the entire test suite, `npm run test` while the `Listening on port 4000` terminal is active.

Test were added to:

-   Verify SQL injection doesn't work
-   Ensure latitude/longitude values are between -90 and 90
-   Show that coordinates provide a more confident autocomplete suggestion
-   Confirm a 429 Too Many Requests error code is possible

---

## Discussion

### Why Prisma?

Prisma makes it really easy to both migrate schemas and seed databases. There's also great TypeScript integration that is baked in to schema definitions.

Prisma can integrate with many different databases and QL languages and generally does a good job at serving as a query builder. In my case, I still had to use $queryRaw, which is just SQL, but there are great features to it that allows for interpolating variables and preventing SQL injections.

### Why Docker?

I thought it would be interesting and fun to make something with it, as I feel like there's a lot of hype around containerization and wanted to see what the hubbub was all about. Once things were set up, it's certainly easy to get going. Hopefully it's easy for you to get started now, too!

### Why Google Cloud Platform?

GCP let me open up an SSH terminal on a virtual Debian machine in the browser and then install Docker via the command line. I then spun up a simple Postgres image. From my Linux machine, I made a VM running Linux that itself was running Linux in Docker. It's Linuxes all the way down!

I knew this option would be free because the VM itself has low enough specs to be within the free tier. I felt like Google would be a reliable choice. Since I don't need to persist my data, most choices would have been fine, but I can apparently still persist with this option, too.

---

# ???

It would've been fun to try:

-   Multiple containers to _'horizontally scale'_ and thereby mitigate high levels of traffic. Each instance of a database/server could individually handle requests, so the load is more evenly distributed.
