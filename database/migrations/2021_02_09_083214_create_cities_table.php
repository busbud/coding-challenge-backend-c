<?php

use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Support\Facades\Schema;

class CreateCitiesTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('cities', function (Blueprint $table) {
            $table->integer('geonameid');
            $table->string('name', 200);
            $table->string('ascii_name', 200);
            $table->string('alternate_names', 5000);
            $table->decimal('latitude');
            $table->decimal('longitude');
            $table->char('feature_class', 1);
            $table->string('feature_code', 10);
            $table->string('country_code ', 10);
            $table->string('cc2', 60);
            $table->string('admin1_code', 20);
            $table->string('admin2_code', 80);
            $table->string('admin3_code', 20);
            $table->string('admin4_code', 20);
            $table->bigInteger('population');
            $table->integer('elevation');
            $table->integer('dem');
            $table->string('timezone',40);
            $table->time('modification_date');
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::dropIfExists('cities');
    }
}