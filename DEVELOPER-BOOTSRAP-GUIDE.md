# Developer  Bootstrap Starter
the solution is based on elasticsearch, logstash and kibana.

we've used also docker and docker compose.
## prerequisites
 - node 10.x (used because 0.x are not available on most distribution)
 - linux distribution (some command assume that you are on linux)
 - docker and docker-compose (we are using docker and docker compose) 
## start elasticsearch & kibana:
go to the root folder of the git repository

run this command ``docker-compose up``

check in your browser if it's ok by accessing these URLS :
- kibana url : ``http://localhost:5601``
- elasticsearch url: ``http://localhost:9200``

copy content of the file ``create-template-index.txt`` in ``console`` kibana.

execute it ``console`` kibana.

for more information about console kibana https://www.elastic.co/guide/en/kibana/current/devtools-kibana.html

run this cmd at root folder to inject data

``docker run -v $(pwd)/elasticsearch/logstash-config:/usr/share/logstash/pipeline -v $(pwd)/data:/usr/share/logstash/input-data -e LS_JAVA_OPTS="-Xmx256m -Xms256m" --network container:elasticsearch docker.elastic.co/logstash/logstash:7.8.0``

run this command to start application

``npm start``

be careful most test depends on running elasticsearch locally

enjoy:)

## consulting data

configure your index pattern on kibana:

https://www.elastic.co/guide/en/kibana/current/tutorial-define-index.html

use the pattern ``cities*``

don't use the ``ModificationDate`` as timestamp


## to Stop elasticsearch and kibana:
``docker-compose down``

## if you'd like to go with clean state run this command
``docker-compose down -v --remove-orphans``



## troubleshooting starting elasticsearch

if you have this error on elasticsearch logs :

``elasticsearch    | [1]: memory locking requested for elasticsearch process but memory is not locked``

``elasticsearch    | [2]: max virtual memory areas vm.max_map_count [65530] is too low, increase to at least [262144]``

run this command to increase you virtual memory:

``sudo sysctl -w vm.max_map_count=262144``
