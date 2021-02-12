<?php

Route::group(['prefix' => 'v1'], function () {
    Route::get('suggestions',array('as'=>'suggestions','uses'=>'AutoCompleteController@suggestions'));
});