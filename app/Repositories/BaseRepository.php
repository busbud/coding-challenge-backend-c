<?php

namespace App\Repositories;

use App\Repositories\Contracts\BaseRepositoryContract;
use Illuminate\Database\Eloquent\Builder;
use Illuminate\Database\Eloquent\Model;

/**
 * Class BaseRepository
 * @package App\Repositories
 * @SuppressWarnings(PHPMD.TooManyPublicMethods)
 */
class BaseRepository implements BaseRepositoryContract
{
    protected Model $model;

    protected Builder $query;

    public function __construct(Model $model = null)
    {
        if ($model) {
            $this->model = $model;
        }
    }

    public function find($entityId)
    {
        return $this->model->find($entityId);
    }

    public function findWith(int $entityId, array $with)
    {
        return $this->model->with($with)->find($entityId);
    }

    public function firstOrCreate($attributes, $values = [])
    {
        return $this->model->firstOrCreate($attributes, $values);
    }

    public function all()
    {
        return $this->model->all();
    }

    public function paginate($params = [])
    {
        return $this->model->paginate($params);
    }

    public function create($data)
    {
        return $this->model->create($data);
    }

    public function updateOrCreate($attributes, $values = [])
    {
        return $this->model->updateOrCreate($attributes, $values);
    }

    public function update($model, $data)
    {
        $model->fill($data)->save();
        return $model;
    }

    public function destroy($model)
    {
        return $model->delete();
    }

    public function findByAttributes(array $attributes)
    {
        return $this->model->where($attributes)->first();
    }

    public function findManyByAttributes(array $attributes)
    {
        return $this->model->where($attributes)->get();
    }

    public function getModel()
    {
        return $this->model;
    }

    public function setModel($model)
    {
        $this->model = $model;
        return $this;
    }

    public function getQuery()
    {
        return $this->query;
    }
}
