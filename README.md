## geoname-suggestion (Docker is Not Included)

Genome Suggestion is an example Laravel project that has most of the key features of Laravel. Most of the features are listed below:
* Laravel
  * Command
  * Importer
  * Migration
  * Model
  * Cast
  * Repository
  * Opcache
  * Route
  * Controller
  * Request
  * Response
  * Swagger
  * Cache
  * unit Tests
  * Algolia Entegration
* Algolia
* Heroku Deployment

## Deployment

Algolia is used as PaaS to handle geographical searches. You can see details about Algolia geo at **[link](https://www.algolia.com/doc/guides/managing-results/refine-results/geolocation/)**. To make run Laravel with Algolia, **[Laravel/Scout](https://laravel.com/docs/8.x/scout)** is used.

One running sample of this application can be monitored from **[Geo App](http://geo-name.herokuapp.com)**

## Testing

```javascript
docker-compose exec src php artisan test
```

## Api Documentation

Swagger is added to project to list and try api end-points at location url("/") or url("/api/documentation")
