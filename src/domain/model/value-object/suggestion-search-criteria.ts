export class SuggestionSearchCriteria {
    q: string;
    latitude: number;
    longitude: number;

    constructor(q: string, latitude: number, longitude: number) {
        this.q = q;
        this.latitude = latitude;
        this.longitude = longitude;
    }
}