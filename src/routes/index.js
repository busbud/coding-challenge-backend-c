const healthCheckRoutes = require('./healthCheckRoutes');

module.exports = (app) => {
  healthCheckRoutes(app);
};
