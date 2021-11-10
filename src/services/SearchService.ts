import elasticsearch from "elasticsearch";

const client = new elasticsearch.Client({
  hosts: ["http://localhost:9200"],
});

const search = (filters: any) => {

  const query: { [key: string ]: any } = {
    "bool": {
      "should": [
        {
          "rank_feature": {
            "field": "population",
            "log": {
              "scaling_factor": 2
            }
          }
        }
      ],
      "must": [
        { "query_string": { "query": `*${filters.term}*`, "fields": ["name"], "boost": 3 }},
        {
          "bool": {
            "should": [
              { "match": { "country_code": "US" } },
              { "match": { "country_code": "CA" } }
            ]
          }
        }
      ]
    }
  };

  if (filters.location) {
    query.bool.should.push({
      distance_feature: {
        field: "location",
        origin: filters.location,
        pivot: "100km"
      }
    });
  }

  const body = {
    "size": filters.limit,
    "from": filters.offset,
    "sort": [{ "_score": { "order": "desc" } }],
    "query": { "function_score": { query }}
  };
  return client.search({ index: "cities_index", body, type: "city_record" });
};

export default ({
  search
})
