<?php

namespace App\Commands;

use App\Repositories;
use Elasticsearch\Client;
use Illuminate\Console\Command;

class ReIndexCities extends Command
{
    /**
     * The name and signature of the console command.
     *
     * @var string
     */
    protected $signature = 'search:cities:rebuild';

    /**
     * The console command description.
     *
     * @var string
     */
    protected $description = 'Reindexes Cities';

    /**
     * @var \Elasticsearch\Client
     */
    private $client;

    /**
     * Create a new command instance.
     *
     * @param \Elasticsearch\Client $client
     */
    public function __construct(Client $client)
    {
        parent::__construct();
        $this->client = $client;
    }

    /**
     * Execute the console command.
     *
     * @return mixed
     */
    public function handle()
    {
        $this->info('Indexing all cities');

        /** @var City $city */
        foreach (City::cursor() as $city) {
            $this->client->index([
                'index' => $city->getSearchIndex(),
                'type' => $city->getSearchType(),
                'id' => $city->id,
                'body' => $city->toSearchArray(),
            ]);

            // PHPUnit-style feedback
            $this->output->write('.');
        }

        $this->info("\nDone!");
    }
}
