const DataTypes = require('sequelize').DataTypes;
const connection = require('../utils/poolConnection');

const GeoNameModel = require('../models/GeoName').init(connection, DataTypes);

const GeoNameService = require('../services/GeoNameService');

const geoNameServiceInstance = new GeoNameService(GeoNameModel, connection);

module.exports = {
	GeoNameService: geoNameServiceInstance,
};