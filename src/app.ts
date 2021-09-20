import { Server } from 'api/server';
import { createServer } from 'http';
import express from 'express';

const app = express();
new Server(app)
  .init()
  .then(app => createServer(app)) 
  .catch((error) => console.error(error));

module.exports = app;
