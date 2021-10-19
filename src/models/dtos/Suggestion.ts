import { Exclude } from "class-transformer"
import { calculateDistance } from "lib/calculateDistance"
import { calculateScore } from "lib/calculateScore"
import { calculateWordScore } from "lib/calculateWordScore"
import { City } from "models/entities/City"
import { ISuggestion } from "models/interfaces/ISuggestion"

export class Suggestion implements ISuggestion {

    name: string
    latitude: string
    longitude: string
    @Exclude()
    wordScore: number
    @Exclude()
    distance: number
    @Exclude()
    realName: string
    score: number

    public FromEntityCity(city: City, latitude: string, longitute: string, cityQuery: string): ISuggestion {
        this.name = `${city.name}, ${city.province.provinceCode}, ${city.country.countryName}`
        this.realName = city.name
        this.latitude = city.latitude
        this.longitude = city.longitude
        this.wordScore = calculateWordScore(city.name, cityQuery)
        this.distance = calculateDistance(city.latitude, city.longitude, latitude, longitute)
        this.score = calculateScore(this.wordScore, this.distance)

        return this
    }

    public ReHidrateFromCache(latitude: string, longitute: string) {
        this.distance = calculateDistance(this.latitude, this.longitude, latitude, longitute)
        this.score = calculateScore(this.wordScore, this.distance)
    }

}