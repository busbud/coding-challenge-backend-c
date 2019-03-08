const bodyParser                  = require('body-parser');
const express                     = require('express');
const { celebrate, Joi, errors }  = require('celebrate');
const { processFile, searchCity } = require('./utils');

const app = express();

app.use(bodyParser.json());

app.get('/suggestions', celebrate({
    query: {
        q: Joi.string().required(),
        latitude: Joi.number().min(-90).max(90),
        longitude: Joi.number().min(-180).max(180)
    }
}), (req, res) => {
    const params = req.query;
    processFile( (data) => {
        cities = searchCity(data, params.q, params.latitude, params.longitude);

        if (Object.keys(cities).length === 0) {
            return res.status(404).json({'suggestions': []});
        }

        return res.status(200).json({'suggestions':cities});
    });
});

app.use(errors());

const port = process.env.PORT || 2345;

app.listen(port, function() {
    console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = app;
