import 'jest';
import {expect} from 'chai';
import cityController from "../../src/controller/city-controller";
import {cityRepository} from '../../src/repository/city'

const cities = [{
  id: 7303783,
  admin1: 7,
  admin2: 0,
  admin3: 0,
  admin4: 0,
  alt_name: "",
  ascii: "Sydney Mines",
  cc2: "",
  country: "CA",
  dem: 18,
  elevation: 0,
  feat_class: "P",
  feat_code: "PPL",
  lat: 46.23669,
  long: -60.21767,
  modified_at: "2010-07-18",
  name: "Londre",
  population: 7312,
  tz: "America/Glace_Bay"
},
  {
    id: 7303783,
    admin1: 7,
    admin2: 0,
    admin3: 0,
    admin4: 0,
    alt_name: "",
    ascii: "Sydney Mines",
    cc2: "",
    country: "CA",
    dem: 18,
    elevation: 0,
    feat_class: "P",
    feat_code: "PPL",
    lat: 46.23669,
    long: -60.21767,
    modified_at: "2010-07-18",
    name: "London",
    population: 7312,
    tz: "America/Glace_Bay"
  }]

describe('CityController', () => {
  it('should be able to filter cities by name given a substring', () => {
    cityRepository.setCities(cities)

    const response = cityController.findAllCitiesScores("lon")
    expect(response.length).to.equals(2)
  })
})
