HEROKU_APP=geosuggest

info:
	heroku info --app $(HEROKU_APP)

ps:
	heroku ps --app $(HEROKU_APP)

deploy:
	git push heroku master

console:
	heroku run console --app $(HEROKU_APP)

view-logs:
	heroku logs --app $(HEROKU_APP)

stream-logs:
	heroku logs --app $(HEROKU_APP) -t

env:
	heroku config --app $(HEROKU_APP)

tree:
	tree -I 'node_modules|cache|test_*'

restart:
	heroku restart --app $(HEROKU_APP)