<?php

use Illuminate\Database\Seeder;
use Carbon\Carbon;

class SeedCityListToDatabase extends Seeder
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function run ()
    {
        $file = new SplFileObject ('data/cities_canada_usa.csv');

        $file->setFlags(
            \ SplFileObject :: READ_CSV |
            \ SplFileObject :: READ_AHEAD |
            \ SplFileObject :: SKIP_EMPTY |
            \ SplFileObject :: DROP_NEW_LINE
        );
        $is_first = true;
        $now = Carbon:: now();
        foreach ($file as $line) {
            if ($is_first === true) {
                $is_first = false;
                continue;
            }

            $geonameId = $line[0];
            $name = $line[1];
            $asciiName = $line[2];
            $alternateNames = $line[3];
            $latitude = $line[4];
            $longitude = $line[5];
            $featureClass = $line[6];
            $featureCode = $line[7];
            $countryCode = $line[8];
            $cc2 = $line[9];
            $admin1Code= $line[10];
            $admin2Code= $line[11];
            $admin3Code= $line[12];
            $admin4Code= $line[13];
            $population = $line[14];
            $elevation = $line[15];
            $dem = $line[16];
            $timezone = $line[17];
            $modificationDate = $line[18];

            $list = [
                'geonameId' => $geonameId,
                'name' => $name,
                'asciiName' => $asciiName,
                'alternateNames' => $alternateNames,
                'latitude' => $latitude,
                'longitude' => $longitude,
                'featureClass' => $featureClass,
                'featureCode' => $featureCode,
                'countryCode' => $countryCode,
                'cc2' => $cc2,
                'admin1Code' => $admin1Code,
                'admin2Code' => $admin2Code,
                'admin3Code' => $admin3Code,
                'admin4Code' => $admin4Code,
                'population' => $population,
                'elevation' => $elevation,
                'dem' => $dem,
                'timezone' => $timezone,
                'modificationDate' => $modificationDate,
            ];

            DB::table("cities")->insert($list);
        }
    }
}
