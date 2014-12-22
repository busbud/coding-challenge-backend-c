#!/bin/bash

echo "Running Apache Benchmark"

ab -c 25 -n 2000 -g output_local.gp 'http://127.0.0.1:2345/suggestions?q=ken'

echo "Done. Now running gnuplot on the results"

gnuplot gp_script_local

echo "Done. Now you can move the resulting output_local.gp and output_local.jpg files for safekeeping"
