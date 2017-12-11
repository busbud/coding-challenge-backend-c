#!/bin/bash
echo "Setup a local Postgresql database for coding challenge"

# default value
USER=busbuduser
DATABASE=busbuddb
PORT=5432
HOST=localhost
PASSWORD=password

while getopts d:u:p:h:e: option
do
 case "${option}"
 in
 u) USER=${OPTARG};;
 d) DATABASE=${OPTARG};;
 p) PORT=${OPTARG};;
 h) HOST=${OPTARG};;
 e) PASSWORD=${OPTARG};;
 esac
done

if [ "$USER" == "postgres" ]; then
	echo "should use another user than 'postgres'"
	exit 1;
fi


if [ "$DATABASE" == "postgres" ]; then
	echo "should use another database name than 'postgres'"
	exit 1;
fi

#drop database if exists
dropdb --if-exists $DATABASE

#drop user
dropuser --if-exists $USER

#create database if exists
createdb $DATABASE
echo "database $DATABASE was created successfully :-)";

# create user
createuser $USER --createdb --superuser
psql -c "ALTER USER $USER WITH PASSWORD '$PASSWORD';"
echo "user $USER was created successfully :-)";

# grant all privileges on database to user 
psql -c "grant all privileges on database $DATABASE to $USER;"

# create schema & import data
psql -p $PORT -U $USER -d $DATABASE -a -f ./create.sql 2>&1 > /dev/null

# create functions
psql -p $PORT -U $USER -d $DATABASE -a -f ./scoring_latlng_name.sql 2>&1 > /dev/null
psql -p $PORT -U $USER -d $DATABASE -a -f ./scoring_latlng.sql 2>&1 > /dev/null
psql -p $PORT -U $USER -d $DATABASE -a -f ./scoring_name.sql 2>&1 > /dev/null

# create .env
sed -e "s;%USER%;$USER;g" -e "s;%PASSWORD%;$PASSWORD;g" -e "s;%HOST%;$HOST;g" -e "s;%PORT%;$PORT;g" -e "s;%DATABASE%;$DATABASE;g" ../.env.template > ../.env


echo "Well done - local database was created successfully"
