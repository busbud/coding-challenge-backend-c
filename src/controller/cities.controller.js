import citiesModel from './../models/cities.model';

const citiesController = {
    suggestions: (request, response) => {
        return response.end(JSON.stringify(
            citiesModel.getSuggestions(request)
        ));
    }
};

export default citiesController;