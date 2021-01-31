const ActuatorRouter = require('express').Router();
const ActuatorController = require('../controller/actuatorController');

/**
 * This endpoint returns the application health status
 */
ActuatorRouter.get('/health', ActuatorController.health);

/**
 * This endpoint returns prometheus application metrics
 */
ActuatorRouter.get('/prometheus', ActuatorController.prometheus);

module.exports = ActuatorRouter;
