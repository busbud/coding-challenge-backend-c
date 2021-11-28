require('dotenv').config();
const db = require('./config/db');
const app = require('./server/ApiServer');

db.authenticate()
  .then(() => console.log('Database connected...'))
  .catch((e) => {
    console.log(`Error connecting database: ${e.message}`);
  });

const { PORT = 3000 } = process.env;

app.listen(PORT, () => {
  console.log(`Server is running on port ${PORT}.`);
});
