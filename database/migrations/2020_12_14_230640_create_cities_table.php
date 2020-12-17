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
            $table->id();
            $table->string('name');
            $table->string('ascii');
            $table->text('alt_name');
            $table->double('lat');
            $table->double('long');
            $table->string('feat_class');
            $table->string('feat_code');
            $table->string('country');
            $table->string('cc2');
            $table->string('admin1');
            $table->string('admin2');
            $table->string('admin3');
            $table->string('admin4');
            $table->integer('population');
            $table->string('elevation');
            $table->integer('dem');
            $table->string('tz');
            $table->string('modified_at');
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
