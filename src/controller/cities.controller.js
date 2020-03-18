import citiesModel from './../models/cities.model';

const citiesController = {
    cities: () =>{
        return citiesModel.getAllCities();
    }
};

export default citiesController;