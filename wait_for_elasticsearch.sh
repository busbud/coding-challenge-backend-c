#!/bin/bash
DEFAULT_ES_NODE="http://localhost:9200/"

ElasticsearchNode=${ES_NODE:-$DEFAULT_ES_NODE}

command="curl --output /dev/null --silent --head --fail ${ElasticsearchNode}"

echo "Wating for elasticsearch at node ${ElasticsearchNode}"
until $($command);	do
  printf '.'
	sleep 5
done
