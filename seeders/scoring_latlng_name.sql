CREATE OR REPLACE FUNCTION scoring_latlng_name(name1 text, name2 text, lat_from numeric, lng_from numeric, lat_to numeric, lng_to numeric) 
RETURNS DECIMAL AS $$
DECLARE score1 decimal;
DECLARE score2 decimal;
BEGIN
	SELECT scoring_latlng(lat_from, lng_from, lat_to, lng_to ) INTO score1;
	SELECT levenshtein(name1, name2) INTO score2;

	return score1*0.8 + score2*0.2;
END;
$$ LANGUAGE plpgsql IMMUTABLE