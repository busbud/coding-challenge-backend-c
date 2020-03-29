import dotenv from 'dotenv';

dotenv.config();

export default {
  esClientConfig: {
    node: process.env.ES_NODE || 'http://localhost:9200'
  }
};
