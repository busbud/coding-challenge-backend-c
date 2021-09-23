const { createServer } = require('./create-server');

const PORT = process.env.PORT ?? 2345;

const app = createServer();

app.listen(PORT);
