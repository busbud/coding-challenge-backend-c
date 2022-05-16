export default {
  definition: {
    openapi: '3.0.3',
    info: {
      title: 'Busbud Challenge - HTTP API Documentation',
    },
    servers: [
      {
        url: '/',
      },
    ],
  },
  apis: ['doc/**/*.yml'],
};
