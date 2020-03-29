importDataToEs:
	echo "Create Index And Importing data to elasticsearch"
	docker-compose run --rm app npm run import:data createIndex ./data/cities_canada-usa.tsv
killAllServices:
	echo "Killing services...."
	docker-compose kill
cleanup:
	echo "Clean up all services...."
	docker-compose down -v --rmi all --remove-orphans
startdev:
	echo "start development"
	docker-compose -f docker-compose.yml -f docker-compose.dev.yml up
deploy:
	echo "Deploy"
