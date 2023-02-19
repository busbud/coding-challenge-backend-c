const locationService = require('../services/location.service');

module.exports = {
    getSuggestions(req, res) {
        try {
            const { q: location, longitude, latitude } = req.query;
            const suggestions = locationService.getSuggestions(location, { latitude, longitude });
            suggestions.length ? res.status(200).json({ suggestions }) : res.status(404).json({ suggestions });
        } catch (err) {
            console.log(err)
            res.status(500).json({ error: 'Unable to retrieve suggestions at this time' });
        }
    }
}