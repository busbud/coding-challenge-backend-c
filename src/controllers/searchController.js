const { StatusCodes } = require('http-status-codes');

const GeoNameService = require('../services').GeoNameService;

module.exports = {
  search: async(req, res) => {
    try {
	    const query = req.query.q || '';
	    const { latitude, longitude } = req.params;
	    let results;

	    if (latitude && longitude) {
		    results = await GeoNameService.findWithPositions(query, latitude, longitude);
      } else {
		    results = await GeoNameService.findWithText(query);
      }

	    return res.status(StatusCodes.OK).json(results);
    } catch (error) {
	    return res.status(StatusCodes.INTERNAL_SERVER_ERROR).json(error);
    }
  },
};
