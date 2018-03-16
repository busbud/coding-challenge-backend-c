#!/usr/bin/env bash
IMAGENAME=bus_bud_challenge_backend
# Get latest git hash to add to image name
tag=$(git log --format="%H" -n 1)
TAG=$IMAGENAME:${tag:0:5}
docker build -t $TAG -t $IMAGENAME:latest .
echo "Built docker image with tag: " $TAG
