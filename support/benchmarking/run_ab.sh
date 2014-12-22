#!/bin/bash

echo "Running Apache Benchmark"

ab -c 5 -n 100 -g output.gp 'https://vast-hollows-6270.herokuapp.com/suggestions?q=ken'

echo "Done. Now running gnuplot on the results"

gnuplot gp_script

echo "Done. Now you can move the resulting output.gp and output.jpg files for safekeeping"
