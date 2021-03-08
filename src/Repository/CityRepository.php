<?php

namespace App\Repository;

use App\Entity\City;
use Doctrine\Bundle\DoctrineBundle\Repository\ServiceEntityRepository;
use Doctrine\Persistence\ManagerRegistry;

/**
 * @method City|null find($id, $lockMode = null, $lockVersion = null)
 * @method City|null findOneBy(array $criteria, array $orderBy = null)
 * @method City[]    findAll()
 * @method City[]    findBy(array $criteria, array $orderBy = null, $limit = null, $offset = null)
 */
class CityRepository extends ServiceEntityRepository
{
    public function __construct(ManagerRegistry $registry)
    {
        parent::__construct($registry, City::class);
    }

    /**
     * @param string $query
     *
     * @return City[]
     */
    public function getCitiesByQuery(string $query): array
    {
        $qb = $this->createQueryBuilder('c')
            ->andWhere('c.name LIKE :query')
            ->andWhere('c.population > :population')
            ->setParameters([
                'query' => '%' . $query . '%',
                'population' => 5000
            ])
        ;

        return $qb->getQuery()->getResult();
    }
}
