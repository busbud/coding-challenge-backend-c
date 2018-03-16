#!/usr/bin/env bash
#NOTE you must be logged in to Heroku registry to be able to push this image :
#token="<your_api_key_here>"
#docker login --username=_ --password=$token registry.heroku.com

appname="gabidi-bus-bud"

docker tag bus_bud_challenge_backend:latest registry.heroku.com/$appname/web
docker push registry.heroku.com/$appname/web