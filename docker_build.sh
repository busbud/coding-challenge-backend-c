#!/bin/bash
IMAGENAME=busbud_challenge_brunorey
tag=$(git log --format="%H" -n 1)
TAG=$IMAGENAME:${tag:0:5}
docker build -t $TAG -t brunorey/$IMAGENAME:latest .
echo "Built docker image with tag: " $TAG

