CREATE OR REPLACE FUNCTION levenshtein(s text, t text) 
RETURNS DECIMAL AS $$
DECLARE i integer;
DECLARE j integer;
DECLARE m integer;
DECLARE n integer;
DECLARE d integer[];
DECLARE c integer;
DECLARE result integer;
DECLARE max_len integer;

BEGIN
	m := char_length(s);
	n := char_length(t);

	i := 0;
	j := 0;

	FOR i IN 0..m LOOP
		d[i*(n+1)] = i;
	END LOOP;

	FOR j IN 0..n LOOP
		d[j] = j;
	END LOOP;

	FOR i IN 1..m LOOP
		FOR j IN 1..n LOOP
			IF SUBSTRING(s,i,1) = SUBSTRING(t, j,1) THEN
				c := 0;
			ELSE
				c := 1;
			END IF;
			d[i*(n+1)+j] := LEAST(d[(i-1)*(n+1)+j]+1, d[i*(n+1)+j-1]+1, d[(i-1)*(n+1)+j-1]+c);
		END LOOP;
	END LOOP;
 
	result := d[m*(n+1)+n];
	max_len := GREATEST(m, n);

	return round ((max_len - result)::decimal/max_len, 2)::decimal;
END;
$$ LANGUAGE plpgsql IMMUTABLE
