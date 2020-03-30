#!/bin/bash

until $(curl --output /dev/null --silent --head --fail http://localhost:9200/);	do
  printf '.'
	sleep 5
done
