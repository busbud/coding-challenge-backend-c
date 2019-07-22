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