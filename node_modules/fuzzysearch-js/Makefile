lint:
	@./node_modules/.bin/jshint \
		./js/*

test: lint
	@./node_modules/.bin/mocha --reporter spec \
		test/*

.PHONY: test