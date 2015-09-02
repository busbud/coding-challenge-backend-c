var elasticUrl = 'http://localhost:9200';

if (process.env.SEARCHBOX_SSL_URL) {
    elasticUrl = process.env.SEARCHBOX_SSL_URL;
}

if (process.env.ELASTIC_PORT_9200_TCP_ADDR && process.env.ELASTIC_PORT_9200_TCP_PORT) {
    elasticUrl = 'http://' + process.env.ELASTIC_PORT_9200_TCP_ADDR + ':' + process.env.ELASTIC_PORT_9200_TCP_PORT;
}

export default {
    ELASTIC_URL: elasticUrl
};
