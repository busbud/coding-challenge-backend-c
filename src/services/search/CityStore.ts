import City from "../../types/City";

class CityStore {

    private _cities: City[] = [];

    constructor(cities: City[]) {
        this._cities = cities;
    }

    get cities(): City[] {
        return this._cities;
    }
}

export default CityStore;