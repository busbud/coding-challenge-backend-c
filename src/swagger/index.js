const swaggerJsdoc = require('swagger-jsdoc');

const swaggerDefinition = {
    openapi: '3.0.0',
    info: {
        title: 'Suggestion API',
        version: '1.0.0',
        description: 'Location suggestion API for the backend coding challenge of busbud',
    },
    servers: [
        {
            url: 'http://localhost:8080',
            description: 'Local server',
        },
        {
            url: 'https://pure-sands-75942.herokuapp.com',
            description: 'Development server',
        },
    ],
};

const options = {
    swaggerDefinition,
    apis: ['./src/router/*.js'],
};

const swaggerSpecification = swaggerJsdoc(options);

module.exports = {
    swaggerSpecification,
};
