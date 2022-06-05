// db configuration file
module.exports = {
  user: process.env.DB_USER,
  host: '127.0.0.1',
  database: 'suggestions',
  password: process.env.DB_PASSWORD,
  port: process.env.DB_PORT,
}
