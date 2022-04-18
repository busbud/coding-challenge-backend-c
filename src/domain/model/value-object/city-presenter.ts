export class CityPresenter {
    name: string;
    latitude: number;
    longitude:number;
    score: number;

    constructor(name: string, latitude: number, longitude:number, score: number) {
        this.name = name;
        this.latitude = latitude;
        this.longitude = longitude;
        this.score = score;
    }
}
