const { createServer } = require('./app');

const port = process.env.PORT ?? 2345;

const app = createServer();

app.listen(port, () => {
  console.log(`Server listening on port ${port}`);

  console.log(`PGUSER: ${process.env.PGUSER}`);
  console.log(`PGHOST: ${process.env.PGHOST}`);
  console.log(`PGPASSWORD: ${process.env.PGPASSWORD}`);
  console.log(`PGDATABASE: ${process.env.PGDATABASE}`);
  console.log(`PGPORT: ${process.env.PGPORT}`);

  console.log(`DATABASE_URL: ${process.env.DATABASE_URL}`);
});
