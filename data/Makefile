all: cities-canada-usa.txt

cities-canada-usa.txt: cities5000.txt
	awk -F'\t' '$$9 == "CA" || $$9 == "US"' $< > $@

cities5000.txt: cities5000.zip
	unzip -o $<
	touch $@

cities5000.zip:
	wget http://download.geonames.org/export/dump/$@
