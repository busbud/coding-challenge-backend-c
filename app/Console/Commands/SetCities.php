<?php

namespace App\Console\Commands;

use Illuminate\Console\Command;
use Illuminate\Support\Facades\Storage;
use App\Repositories\CityRepositoryInterface;

class SetCities extends Command
{
    /**
     * The name and signature of the console command.
     *
     * @var string
     */
    protected $signature = 'set:cities';

    /**
     * The console command description.
     *
     * @var string
     */
    protected $description = 'Let me set all cities data into database !';

    /**
     * Create a new command instance.
     *
     * @return void
     */
    public function __construct()
    {
        parent::__construct();
    }

    public function handle(
        CityRepositoryInterface $cityRepository
    ) {
        $cityRepo = $cityRepository;
        $fileLink = Storage::disk('public')->path('/cities_canada-usa.tsv');
        $tsvArr = file($fileLink);

        //Get rid of title row
        array_shift($tsvArr);

        $progressBar = $this->output->createProgressBar(count($tsvArr));
        $progressBar->start();

        foreach ($tsvArr as $rowIndex => $eachRow) {
            $eachRow = explode("\t", $eachRow);
            $cityRepo->parseTsvData($eachRow);
            $progressBar->advance();
        }

        $cityRepo->saveData();
        //$cityRepo->setDataToRedis();

        $progressBar->finish();
    }
}
