#! /bin/bash
data=$1
field_file=$2

mongoimport --type tsv --collection locations --file $data --fieldFile $field_file
