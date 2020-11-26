<?php

namespace App\Providers;

use App\Models\Geoname;
use App\Repositories\Contracts\GeonameRepositoryContract;
use App\Repositories\GeonameRepository;
use Illuminate\Support\ServiceProvider;
use Laravel\Scout\Builder;

class AppServiceProvider extends ServiceProvider
{
    /**
     * Register any application services.
     *
     * @return void
     */
    public function register()
    {
        $this->app->register(\L5Swagger\L5SwaggerServiceProvider::class);
    }

    /**
     * Bootstrap any application services.
     *
     * @return void
     */
    public function boot()
    {
        $this->app->bind(GeonameRepositoryContract::class, function () {
            return new GeonameRepository(new Geoname());
        });
    }
}
