VERSION_FILE=VERSION
VERSION=`cat $(VERSION_FILE)`

all: compile

deps:
	sudo npm install --global babel gulp
	npm install

gulp:
	gulp

run:
	make run-both -j2

run-both: run-gulp run-nodemon

run-gulp:
	gulp watch

run-nodemon:
	nodemon ./server.js

import:
	babel-node ./data/importCities.js

test:


.PHONY: run
