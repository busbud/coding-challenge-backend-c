const { Client: Client7 } = require('es7');
const parser = require('../utils').TsvUtils;

function create_client() {
    return new Client7({ node: 'http://localhost:9200' });
}

async function init_module() {
    const client = create_client();
    try {
        //If the elastic search index does not exist then create it
        const result = await client.indices.exists({ index: 'population' });
        if (!result.body) {
            console.log("Index does not exist");
            await client.indices.create({
                "index": "population",
                body: {
                    "mappings": {
                        "properties": {
                            "location": { "type": "geo_point" }
                        }
                    }
                }
            });

            console.log("Created new indicies");
        }

        //Update regardless
        await update_population_index('data/cities_canada-usa.tsv');
    }
    catch (e) {
        console.log(e);
    }
}
const update_population_index = async (tsvFile) => {
    const query = await parser.parse(tsvFile);
    const client = create_client();
    await client.bulk({
        index: 'population',
        body: query
    });
    console.log("Updated index");
}

const get_suggestions = async (query) => {
    const client = create_client();
    const result = await client.search(query);
    return result.body.hits.hits.map(s =>
        ({
            "name": `${s._source.name}, ${s._source.admin1}, ${s._source.country}`,
            "latitude": s._source.lat,
            "longitude": s._source.long,

            //The highest element score is the best match at 1
            "score": s._score / result.body.hits.max_score
        }));
};

module.exports = {
    get_suggestions: get_suggestions,
    update_population_index: update_population_index
}

init_module();