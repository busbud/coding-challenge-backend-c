#!/bin/sh
# script for starting built-in php server.
# IMPORTANT : Only for development.
#             Do not deploy to production.
#             see README.md inside ./web folder

path=$(dirname $(readlink -fn -- ${0}))
php -S localhost:8000 $path/app.php
