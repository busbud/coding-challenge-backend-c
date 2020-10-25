const app = require('./app');
const config = require('config');

const port = process.env.PORT || 2345;

app.set('trust proxy', '127.0.0.1');

app.listen(port, () => {
  console.log(`App listening on port : ${port}`);
});