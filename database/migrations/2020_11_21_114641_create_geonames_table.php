<?php

use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Support\Facades\Schema;

class CreateGeonamesTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('geonames', function (Blueprint $table) {
            $table->id();
            $table->string('name', 200);
            $table->string('asciiname', 200);
            $table->text('alternatenames')->nullable();
            $table->double('latitude');
            $table->double('longitude');
            $table->char('feature_class', 1)->nullable();
            $table->string('feature_code', 10)->nullable();
            $table->string('country_code', 2)->nullable();
            $table->string('cc2', 60)->nullable();
            $table->string('admin1_code', 20)->nullable();
            $table->string('admin2_code', 80)->nullable();
            $table->string('admin3_code', 20)->nullable();
            $table->string('admin4_code', 20)->nullable();
            $table->bigInteger('population')->nullable();
            $table->integer('elevation')->nullable();
            $table->integer('dem')->nullable();
            $table->string('timezone')->nullable();
            $table->date('modification_date')->nullable();
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::dropIfExists('geonames');
    }
}
