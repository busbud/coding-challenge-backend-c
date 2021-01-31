/** Holds queries which used by services
 * @module db/queries
 */
module.exports = {
  /**
   * Search query.
   */
  searchWord: `SELECT 
    name || ', ' || admin1 || ', ' || country AS name,
    lat AS latitude,
    long AS longitude,
    ROUND(CUME_DIST() OVER ( ORDER BY queryRank DESC )::DECIMAL, 3) score
  FROM (
    SELECT 
      l.name,
      l.admin1,
      l.country,
      l.lat,
      l.long,
      ts_rank_cd(tsv, searchToQuery, 32) AS queryRank
    FROM locations l, plainto_tsquery('english', $1) searchToQuery
    WHERE tsv @@ searchToQuery
    LIMIT 20
    ) as searchQuery ORDER BY score DESC;`,
  /**
   * Search query by location info. While only nearest results affects score, there is no need spatial calculation.
   */
  searchWordWithLocationScore: `SELECT 
      name || ', ' || admin1 || ', ' || country AS name,
      lat AS latitude,
      long AS longitude,
      (distanceScore * 0.2 + score * 0.8) score
    FROM (
    SELECT *, ROUND(CUME_DIST() OVER ( ORDER BY distance )::DECIMAL, 3) distanceScore 
    FROM 
      (
        SELECT *, ( 6371 * acos( cos( radians($3) ) * cos( radians( lat ) ) * 
          cos( radians( long ) - radians($2) ) + sin( radians($3) ) * 
          sin( radians( lat ) ) ) ) AS distance 
        FROM  
          (

            SELECT *, ROUND(CUME_DIST() OVER ( ORDER BY queryRank DESC )::DECIMAL, 3) score
            FROM (
              SELECT 
                l.name,
                l.admin1,
                l.country,
                l.lat,
                l.long,
                 ts_rank_cd(tsv, searchToQuery, 32) AS queryRank
              FROM locations l, plainto_tsquery('english', $1) searchToQuery
              WHERE tsv @@ searchToQuery
              LIMIT 20
              ) as searchQuery

          ) as querySearch
      ) as locationSearch
      ) as totalScoreCalc ORDER BY score DESC
  `,
};
