import citiesModel from './../models/cities.model';

const citiesController = {
    suggestions: (request, response) => {
        return response.end(
            citiesModel.getSuggestions(request)
        );
    }
};

export default citiesController;