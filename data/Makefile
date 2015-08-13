all: cities-canada-usa.json

HEADERS += geonameid
HEADERS += name
HEADERS += asciiname
# HEADERS += alternatenames
HEADERS += latitude
HEADERS += longitude
# HEADERS += featureClass
# HEADERS += featureCode
# HEADERS += countryCode
# HEADERS += cc2
# HEADERS += admin1Code
# HEADERS += admin2Code
# HEADERS += admin3Code
# HEADERS += admin4Code
HEADERS += population
# HEADERS += elevation
# HEADERS += dem
# HEADERS += timezone
# HEADERS += modificationDate

%.json: %.light.txt
	(echo $(HEADERS) | sed 's/ /\t/g'; cat $<) | ../node_modules/.bin/csvtojson --delimiter=$$'\t' > $@

%.light.txt: %.txt
	awk -F'\t' -v OFS='\t' '{print $$1, $$2, $$3, $$5, $$6, $$15}' $< > $@

cities-canada-usa.txt: cities5000.txt
	awk -F'\t' '$$9 == "CA" || $$9 == "US"' $< > $@

cities5000.txt: cities5000.zip
	unzip -o $<
	touch $@

cities5000.zip:
	wget http://download.geonames.org/export/dump/$@
