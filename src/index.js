require('dotenv').config();

const { createServer } = require('./app');

const port = process.env.PORT ?? 2345;

const app = createServer();

app.listen(port, () => {
  console.log(`Server listening on port ${port}`);
});
