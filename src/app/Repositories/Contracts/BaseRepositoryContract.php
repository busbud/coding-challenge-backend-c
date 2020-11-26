<?php

namespace App\Repositories\Contracts;

interface BaseRepositoryContract
{
    public function find($entityId);

    public function findWith(int $entityId, array $with);

    public function firstOrCreate($attributes, $values = []);

    public function all();

    public function paginate($params = []);

    public function create($data);

    public function updateOrCreate($attributes, $values = []);

    public function update($model, $data);

    public function destroy($model);

    public function findByAttributes(array $attributes);

    public function findManyByAttributes(array $attributes);

    public function getModel();

    public function setModel($model);

    public function getQuery();
}
