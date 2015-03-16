ES_HOST=https://2ek0l4zm:n2wck2urbhr7rf9v@ivy-2031075.us-east-1.bonsai.io


curl -XPOST "$ES_HOST/challenge/location/_search?pretty" -d '
{
    "query":{"fuzzy" : { "name" : "actn" }}
  }
'