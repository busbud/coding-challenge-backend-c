CREATE OR REPLACE FUNCTION scoring_latlng(lat_from numeric, lng_from numeric, lat_to numeric, lng_to numeric) 
RETURNS DECIMAL AS $$
DECLARE km integer;
DECLARE result decimal;
BEGIN
	SELECT (ST_DistanceSphere(ST_MakePoint(lng_from, lat_from ), ST_MakePoint(lng_to, lat_to))/1000) INTO km;

	IF km <= 100 THEN
    	result := 1;
	ELSIF km <= 200 THEN
		result := 0.9;
	ELSIF km <= 300 THEN
    	result := 0.8;
	ELSIF km <= 400 THEN
    	result := 0.7;
	ELSIF km <= 500 THEN
    	result := 0.6;
	ELSIF km <= 600 THEN
    	result := 0.5;
	ELSIF km <= 700 THEN
    	result := 0.4;
	ELSIF km <= 800 THEN
    	result := 0.3;
	ELSIF km <= 900 THEN
    	result := 0.2;
	ELSIF km <= 1000 THEN
    	result := 0.1;
	ELSE 
    	result := 0;
	END IF;

	return result;
END;
$$ LANGUAGE plpgsql IMMUTABLE