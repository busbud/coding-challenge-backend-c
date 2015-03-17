#! /bin/bash
#creates a mongodb called 'location-db' and a imports the .tsv file passed as the first argument. 2nd argument was for simplicity to create the fields, instead of passing them as a stream to mongoimport"

data=$1
field_file=$2

mongoimport --type tsv --collection locations --db "location-db" --file $data --fieldFile $field_file;
