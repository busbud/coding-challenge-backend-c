class GeoNameService {
	constructor(geoNameModel, connection) {
		this.geoNameModel = geoNameModel;
		this.connection = connection;
	}

	async findWithPositions(text, latitude, longitude) {
		const textToQuery = `${text}:*`;
		const query = `SELECT id, name, ascii_name AS "asciiName", country,
          (RANK() OVER (
            ORDER BY location <-> point '(${longitude}, ${latitude})' DESC
           ) * 0.1) AS score
        FROM geo_names 
          WHERE search_tokens @@ to_tsquery('english', :query) 
            LIMIT 10`;
		console.log(query);

		return this.connection.query(query,
			{
				model: this.geoNameModel,
				replacements: {
					query: textToQuery,
					longitude,
					latitude,
				},
			});
	}

	async findWithText(text) {
		const textToQuery = `${text}:*`;
		const query = `SELECT id, name, ascii_name AS "asciiName", country,
          (RANK() OVER (
            ORDER BY id
           ) * 0.1) AS score
        FROM geo_names 
          WHERE search_tokens @@ to_tsquery('english', :query) 
            LIMIT 10`;

		return this.connection.query(query,
			{
				model: this.geoNameModel,
				replacements: {
					query: textToQuery,
				},
			});
	}


}

module.exports = GeoNameService;