import {Client} from 'elasticsearch';

const host = process.env.SEARCHBOX_SSL_URL || ('http://' + process.env.ELASTIC_PORT_9200_TCP_ADDR + ':' + process.env.ELASTIC_PORT_9200_TCP_PORT);

const client = new Client({
  host: host
});

export default client;
