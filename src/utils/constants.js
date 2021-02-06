const constants = {

  db: {
    host: process.env.DB_HOST || 'postgres',
    username: process.env.DB_USER || 'postgres',
    password: process.env.DB_PASSWORD || 'postgres',
    database: process.env.DB_NAME || 'db',
    port: process.env.DB_PORT || 5432,
  },

  app: {
    name: 'Busbud API',
    limitItemsPerPage: process.env.LIMIT_ITEMS_PER_PAGE || 30,
    defaultPagePagination: 0,
  },

  server: {
    port: process.env.PORT || 3000,
  },

};

module.exports = constants;
