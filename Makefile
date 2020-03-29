importDataToEs:
	echo "Create Index And Importing data to elasticsearch"
	docker-compose run --rm app npm run import:data createIndex ./data/cities_canada-usa.tsv

testWithEs:
	echo "Run test unit with elasticsearch"
	echo "Import data"
	docker-compose up -d elasticsearch
	docker-compose run --rm app npm run import:data createIndex ./data/cities_canada-usa.tsv
	docker-compose run --rm app npm run test:es

testUnit:
	docker-compose run --rm app npm run test:unit

killAllServices:
	echo "Killing services...."
	docker-compose kill

cleanup:
	echo "Clean up all services...."
	docker-compose down -v --rmi all --remove-orphans

startdevES:
	echo "start development"
	docker-compose -f docker-compose.yml -f docker-compose.elasticsearch.yml up

startdevFile:
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml up

deploy:
	echo "Deploy"
