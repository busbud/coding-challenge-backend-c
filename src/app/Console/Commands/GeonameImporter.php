<?php

namespace App\Console\Commands;

use App\Repositories\Contracts\GeonameRepositoryContract;
use Illuminate\Console\Command;

class GeonameImporter extends Command
{
    /**
     * The name and signature of the console command.
     *
     * @var string
     */
    protected $signature = 'import:start';

    /**
     * The console command description.
     *
     * @var string
     */
    protected $description = 'This command reads from tvs file and populates the database.';

    private GeonameRepositoryContract $geonameRepository;

    /**
     * Create a new command instance.
     *
     * @param GeonameRepositoryContract $geonameRepository
     */
    public function __construct(GeonameRepositoryContract $geonameRepository)
    {
        $this->geonameRepository = $geonameRepository;
        parent::__construct();
    }

    /**
     * Execute the console command.
     *
     * @return int
     */
    public function handle()
    {
        /* by calling file method, all the file is loaded into memory.
           This is not an optimal solution. However for this kind of small files, it would be harmless.
           For bigger files, you can check the method described in https://josephsilber.com/posts/2020/07/29/lazy-collections-in-laravel
        */
        $file = file(storage_path('app/cities_canada-usa.tsv'));
        $rowCount = count($file);
        $bar = $this->output->createProgressBar($rowCount);
        $bar->start();
        foreach ($file as $key => $line) {

            if ($key == 0) {
                continue;
            }
            $this->convertAndSaveModel($line);
            $bar->advance();
        }
        $bar->finish();

        $this->info("\nData population is done! Now it is time to import to algolia");

        //then import geo data to algolia
        $this->call("scout:import");

        return 0;
    }

    private function convertAndSaveModel(string $line): void
    {
        $data = $this->transform(explode("\t", $line));
        $this->geonameRepository->firstOrCreate(["id" => $data["id"]], $data);
    }

    private function transform(array $data): array
    {
        return [
            'id' => (int)$data[0],
            'name' => $data[1],
            'asciiname' => $data[2],
            'alternatenames' => $data[3],
            'latitude' => (double)$data[4],
            'longitude' => (double)$data[5],
            'feature_class' => $data[6],
            'feature_code' => $data[7],
            'country_code' => $data[8],
            'cc2' => $data[9],
            'admin1_code' => $data[10],
            'admin2_code' => $data[11],
            'admin3_code' => $data[12],
            'admin4_code' => $data[13],
            'population' => (int)$data[14],
            'elevation' => (int)$data[15],
            'dem' => (int)$data[16],
            'timezone' => $data[17],
            'modification_date' => $data[18],
        ];
    }
}
