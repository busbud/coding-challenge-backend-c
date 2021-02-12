<?php

namespace App\Models;

use App\Search\Searchable;
use Illuminate\Database\Eloquent\Model;

class City extends Model
{
    use Searchable;

    public $fillable = [
        'geonameid',
        'name',
        'ascii_name',
        'alternate_names',
        'latitude',
        'longitude',
        'feature_class',
        'feature_code',
        'country_code',
        'cc2',
        'admin1_code',
        'admin2_code',
        'admin3_code',
        'admin4_code',
        'population',
        'elevation',
        'dem',
        'timezone',
        'modification_date'
    ];

    protected $table = 'cities';
}