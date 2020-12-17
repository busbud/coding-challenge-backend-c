<p align="center">Suggestion App</p>

## About /suggestions

This api provides a get request which can be used by the given {q}* = search query {longitude} and {latitute} parameters.

System works over;
Php 8.0
Mysql database

## Return values
- **{name}**
- **{score}** -> Calculated by best match between 1.0 to 0.1
- **{lat}**
- **{long}**


## Available Commands
- **set:cities-** Populates data to database -> fill cities table with the tsv file data by the following command
```
php artisan set:cities
```

## Documentation
- **Swagger** (http://busbud-city-app.herokuapp.com/api/documentation)
