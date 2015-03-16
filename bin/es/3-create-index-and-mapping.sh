ES_HOST=https://2ek0l4zm:n2wck2urbhr7rf9v@ivy-2031075.us-east-1.bonsai.io


echo Creating index...
curl -XPUT "$ES_HOST/challenge?pretty"

echo Creating mapping...
curl -XPUT "$ES_HOST/challenge/_mapping/location" -d '
{
  "location" : {
    "properties" : {
      "name": {"type": "string"},
      "state": {"type": "string"},
      "country": {"type": "string"},
      "coord" : {"type" : "geo_point"}
    }
  }
}'


echo trying to index a document...
curl -XPUT "$ES_HOST/challenge/location/5887470?pretty" -d '
{"id":"5882142","name": "a name", "country":"Canada","state":"QC","coord":"45.65007,-72.56582"}'
