import citiesModel from './../models/cities.model';

const citiesController = {
    suggestions: (request, response) => {
        const result = citiesModel.getSuggestions(request);
        if(result) {
            if(result.suggestions.length > 0){
                response.writeHead(200, { 'Content-Type': 'text/plain' });
            }
        } 

        return response.end(
            JSON.stringify(result)
        );
        
    }
};

export default citiesController;