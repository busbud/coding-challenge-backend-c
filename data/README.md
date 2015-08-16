Data
====

This directory contains (scripts to retrieve/compute) the data needed
for the library/application.

The makefile will retrieve the city and states data from [GeoNames]
and compute a JSON file with the data ready to be used by the
application.

This avoids parsing TSV and computing data everytime the program is
executed.

[GeoNames]: http://download.geonames.org/export/dump/readme.txt

Dependencies
------------

The data retrieval and build logic is implemented with a
[makefile](Makefile), using GNU Make syntax (so you need to use either
`make` or `gmake` depending on your system).

The makefile also relies on GNU awk, the `unzip` command, and GNU Wget.

Without these programs installed, you won't be able to build the
database.

Usage
-----

```sh
make
```

Note this will be automatically executed when you run `npm install`, so
you probably won't have to do this by hand unless you want to recompile
a specific file.
