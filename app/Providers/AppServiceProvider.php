<?php

namespace App\Providers;

use App\Repositories\ICityRepository;
use App\Repositories\CityRepository;
use App\Repositories\ElasticSearchCitiesRepository;
use Elasticsearch\Client;
use Elasticsearch\ClientBuilder;
use Illuminate\Support\ServiceProvider;

class AppServiceProvider extends ServiceProvider
{
    /**
     * Bootstrap any application services.
     *
     * @return void
     */
    public function boot()
    {
        //
    }

    /**
     * Register any application services.
     *
     * @return void
     */
    public function register()
    {
        $this->app->bind(CityRepository::class, ElasticSearchCitiesRepository::class);
        $this->app->bind(Client::class, function () {
            return ClientBuilder::create()
                ->setHosts(config('services.search.hosts'))
                ->build();
        });
    }
}
