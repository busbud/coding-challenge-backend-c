<?php

namespace App\Models;

use Illuminate\Database\Eloquent\Model;

/**
 * @OA\Schema(
 *   @OA\Xml(name="City")
 * )
 */
class City extends Model
{

  /**
   * @OA\Property(format="int64")
   * @var int
   */
  public $id;

  /**
   * @OA\Property()
   * @var string
   */
  public $name;

  /**
   * @OA\Property()
   * @var float
   */
  public $lat;

  /**
   * @OA\Property()
   * @var float
   */
  public $long;

  /**
   * @OA\Property()
   * @var float
   */
  public $score;

  /**
   * The attributes that are mass assignable.
   *
   * @var array
   */
  public $fillable = [
    'id',
    'name',
    'ascii',
    'alt_name',
    'lat',
    'long',
    'feat_class',
    'feat_code',
    'country',
    'cc2',
    'admin1',
    'admin2',
    'admin3',
    'admin4',
    'population',
    'elevation',
    'dem',
    'tz',
    'modified_at'
  ];

  protected $table = 'cities';
}
