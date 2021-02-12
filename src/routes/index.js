const healthCheckRoutes = require('./healthCheckRoutes');
const searchRoutes = require('./searchRoutes');

module.exports = (app) => {
  healthCheckRoutes(app);
	searchRoutes(app);
};
