import {Client} from 'elasticsearch';

var client = new Client({
  host: 'http://' + process.env.ELASTIC_PORT_9200_TCP_ADDR + ':' + process.env.ELASTIC_PORT_9200_TCP_PORT
});

export default client;
