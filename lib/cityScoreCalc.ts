import { City } from "../utils/interfaces";
import { getDistanceFromLatLonInKm } from "./distanceCalculator";

export const  calculateScore = (queryString: string, qLatitude: string, qLongitude: string, cityName: string, city: City): number => {
    let score = 0;

    // console.log(`cityName: ${cityName}`);
    // console.log(`city: ${JSON.stringify(city)}`);

    if (cityName.toLowerCase().startsWith(queryString.toLowerCase())) {
        score = 0.8;
    } else if (cityName.toLowerCase().includes(queryString.toLowerCase())) {
        score = 0.6;
    }

    if (qLatitude && qLongitude) {
        const cityLatitude = city.lat as string;
        const cityLongtitude = city.long as string;
        const distance = getDistanceFromLatLonInKm(parseFloat(qLatitude), parseFloat(qLongitude), parseFloat(cityLatitude), parseFloat(cityLongtitude));
        const distanceScore = 1 / (distance + 1);
        score = score + (distanceScore * 0.2);
    }

    //return 2 decimal places
    return Math.round(score * 100) / 100;
}