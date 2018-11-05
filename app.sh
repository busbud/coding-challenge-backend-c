
RED='\033[0;31m' # Red
GREEN='\033[0;32m' # Green
NC='\033[0m' # No Color

echo '\n'${GREEN}'-------> FOREVER STOP'${NC}'\n'
forever stop 'busbud-challenge@api-geonames'

echo '\n'${GREEN}'-------> UPDATE FROM GIT...'${NC}'\n'
git pull

echo '\n'${GREEN}'-------> REMOVE NODE_MODULES...'${NC}'\n'
rm -r node_modules

echo '\n'${GREEN}'-------> REMOVE PACKAGE-LOCK...'${NC}'\n'
rm -r package-lock.json

echo '\n'${GREEN}'-------> NPM INSTALL...'${NC}'\n'
npm install

echo '\n'${GREEN}'-------> FOREVER START'${NC}'\n'
forever --uid 'busbud-challenge@api-geonames' --append start -c 'node' app.js


