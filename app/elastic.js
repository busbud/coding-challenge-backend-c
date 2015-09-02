import {Client} from 'elasticsearch';
import config from '../config';

const client = new Client({
  host: config.ELASTIC_URL
});

export default client;
