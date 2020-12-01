<?php

namespace App\Imports;

use Maatwebsite\Excel\Concerns\Importable;
use Maatwebsite\Excel\Concerns\ToArray;

class GeonameImport implements ToArray
{
    use Importable;

    /**
     * @inheritDoc
     */
    public function array(array $array)
    {
        // TODO: Implement array() method.
    }
}
