-- Map 2-digit codes to provinces (https://www.geonames.org/CA/administrative-division-canada.html)
UPDATE cities
SET admin1 = 'AB'
WHERE admin1 = '01';

UPDATE cities
SET admin1 = 'BC'
WHERE admin1 = '02';

UPDATE cities
SET admin1 = 'MB'
WHERE admin1 = '03';

UPDATE cities
SET admin1 = 'NB'
WHERE admin1 = '04';

UPDATE cities
SET admin1 = 'NL'
WHERE admin1 = '05';

UPDATE cities
SET admin1 = 'NS'
WHERE admin1 = '07';

UPDATE cities
SET admin1 = 'ON'
WHERE admin1 = '08';

UPDATE cities
SET admin1 = 'PE'
WHERE admin1 = '09';

UPDATE cities
SET admin1 = 'QC'
WHERE admin1 = '10';

UPDATE cities
SET admin1 = 'SK'
WHERE admin1 = '11';

UPDATE cities
SET admin1 = 'YT'
WHERE admin1 = '12';

UPDATE cities
SET admin1 = 'NT'
WHERE admin1 = '13';

UPDATE cities
SET admin1 = 'NU'
WHERE admin1 = '14';