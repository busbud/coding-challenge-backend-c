import winston from 'winston';

const logger = winston.createLogger({
  level: 'debug',
  defaultMeta: { service: 'suggestions-service' },
  transports: [
    new winston.transports.Console({
      format: winston.format.json(),
    }),
  ],
});

export default logger;
