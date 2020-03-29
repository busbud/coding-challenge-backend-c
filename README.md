### Commands

- `make importDataToEs` - Import city data to elasticsearch
- `make testWithEs` - Test ElasticSearch service. Connect directly to elasticsearch - not use nock to mock data 
- `make testUnit` - Run all unit test on docker
- `make killAllServices` - Kill all running services
- `make cleanup` - Clean up all containers, volumne, services which do not use
- `make startdevES` - Start server, using search from elasticsearch
- `make startdevFile` - Start server, use searching from file
- `npm run build` - Build code
- `npm run lintjs` - check code convention 
- `npm run lintjs:fix` - check code convention, and auto fix.

#### Things can be improved if I have more time

- [] Use **lerna** to implement monorepo, so that I can move search services to separated packages. 
Packages that I would like to move to packages:

    - es_search
    - search_test_helper
    - file_search
    ... 

- [] User Joi to validate configuration from environments variables
- [] Add caching for each search keywords   
- [] Write script to wait for elasticsearch service up before run any application command. 
Currently temporary run test by sequence docker, it makes running the test slow       
- [] Configure git hooks by using `husky` to run fixing code convention before each commit.
 
