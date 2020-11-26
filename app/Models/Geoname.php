<?php

namespace App\Models;

use Illuminate\Database\Eloquent\Factories\HasFactory;
use Illuminate\Database\Eloquent\Model;
use Laravel\Scout\Searchable;

class Geoname extends Model
{
    use HasFactory; use Searchable;

    public $timestamps = false;

    protected $fillable = [
        'id',
        'name',
        'asciiname',
        'alternatenames',
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

    protected $casts = [
        'id' => 'integer',
        'name' => 'string',
        'asciiname' => 'string',
        'alternatenames' => 'string',
        'latitude' => 'double',
        'longitude' => 'double',
        'feature_class' => 'string',
        'feature_code' => 'string',
        'country_code' => 'string',
        'cc2' => 'string',
        'admin1_code' => 'string',
        'admin2_code' => 'string',
        'admin3_code' => 'string',
        'admin4_code' => 'string',
        'population' => 'integer',
        'elevation' => 'integer',
        'dem' => 'integer',
        'timezone' => 'string',
        'modification_date' => 'string',
    ];

    public function toSearchableArray()
    {
        return [
            'id' => $this->id,
            'name' => $this->name,
            'asciiname' => $this->asciiname,
            '_geoloc' => [
                'lat' => $this->latitude,
                'lng' => $this->longitude,
            ]
        ];
    }

}
