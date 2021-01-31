<?php

namespace App\Http\Services;

use Goodby\CSV\Import\Standard\Lexer;
use Goodby\CSV\Import\Standard\Interpreter;
use Goodby\CSV\Import\Standard\LexerConfig;

class DataImportService
{
    const FILE_PATH = "cities_canada-usa.tsv";
    const CANADA_STATES = [
        '01' => 'AB',
        '02' => 'BC',
        '03' => 'MB',
        '04' => 'NB',
        '05' => 'NL',
        '07' => 'NS',
        '08' => 'ON',
        '09' => 'PE',
        '10' => 'QC',
        '11' => 'SK',
        '12' => 'YT',
        '13' => 'NT',
        '14' => 'NU'
    ];

    public static function getOrImportCities(): array
    {
        // the result comes into this variable
        $cities = [];

        $config = new LexerConfig();
        $config->setDelimiter("\t");
        $config->setFlags(\SplFileObject::READ_AHEAD | \SplFileObject::SKIP_EMPTY | \SplFileObject::READ_CSV);
        $lexer = new Lexer($config);

        $interpreter = new Interpreter();
        $interpreter->addObserver(function(array $row) use (&$cities) {
            $city = self::getCityFromTsvRow($row);

            if ($city['population'] > 5000) {
                $cities[] = $city;
            }
        });

        $lexer->parse(base_path() . '/public/' . self::FILE_PATH, $interpreter);

        // TODO : set in redis or nosql, and get from there for performance

        return $cities;
    }

    private static function getCityFromTsvRow(array $row) : array
    {
        return [
            "name" => $row[1],
            "shownName" => self::generateCityNameWithProvinceAndCountry($row[1], $row[8], $row[10]),
            "lat" => $row[4],
            "long" => $row[5],
            "country" => $row[8],
            "population" => (int)$row[14]
        ];
    }

    private static function generateCityNameWithProvinceAndCountry(string $name, string $country, string $admin1) : string
    {
        $province = $admin1;
        $countryName = 'USA';

        // If country is canada then convert the admin1 code to province
        if ($country == 'CA') {
            $countryName = 'Canada';
            if (array_key_exists($admin1, self::CANADA_STATES)) {
                $province = self::CANADA_STATES[$admin1];
            }
        }

        return "$name, $province, $countryName";
    }
}
