all: cities-canada-usa.json states.json

cities-%.json: cities-%.txt states.json
	../node_modules/.bin/babel-node cities.js < $< > $@

states.json: admin1CodesASCII.txt
	../node_modules/.bin/babel-node states.js < $< > $@

cities-canada-usa.txt: cities5000.txt
	awk -F'\t' '$$9 == "CA" || $$9 == "US"' $< > $@

cities5000.txt: cities5000.zip
	unzip -o $<
	touch $@

cities5000.zip admin1CodesASCII.txt:
	wget http://download.geonames.org/export/dump/$@
