<?php

namespace Src\System;

class DatabaseConnector
{

    private $dbConnection = null;

    public function __construct()
    {
        $this->dbConnection = new \PDO(
            "mysql:host=db;port=3306;charset=utf8mb4;dbname=geoname",
            "root",
            "root", array(\PDO :: ATTR_ERRMODE => \PDO :: ERRMODE_EXCEPTION)
        );
    }

    public function getConnection()
    {
        return $this->dbConnection;
    }
}