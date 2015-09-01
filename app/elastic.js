import {Client} from 'elasticsearch';

const host = process.env.SEARCHBOX_SSL_URL || (
  process.env.ELASTIC_PORT_9200_TCP_ADDR && process.env.ELASTIC_PORT_9200_TCP_PORT ?
  'http://' + process.env.ELASTIC_PORT_9200_TCP_ADDR + ':' + process.env.ELASTIC_PORT_9200_TCP_PORT : null)
  || 'http://localhost:9200';

const client = new Client({
  host: host
});

export default client;
