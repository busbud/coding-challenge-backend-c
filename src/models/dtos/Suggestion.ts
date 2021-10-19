import { classToPlain, Exclude } from "class-transformer"
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

    public FromEntityCity(city: City, latitude: string, longitude: string, cityQuery: string): ISuggestion {
        this.name = `${city.name}, ${city.province.provinceCode}, ${city.country.countryName}`
        this.realName = city.name
        this.latitude = city.latitude
        this.longitude = city.longitude
        this.wordScore = calculateWordScore(city.name, cityQuery)
        this.distance = calculateDistance(city.latitude, city.longitude, latitude, longitude)
        this.score = calculateScore(this.wordScore, this.distance)
        return this
    }

    public ReHidrateFromCache(latitude: string, longitude: string) {
        this.distance = calculateDistance(this.latitude, this.longitude, latitude, longitude)
        this.score = calculateScore(this.wordScore, this.distance)
    }

    public FromCacheData(cacheObject: any): ISuggestion {
        this.name = cacheObject.name
        this.realName = cacheObject.name
        this.latitude = cacheObject.latitude
        this.longitude = cacheObject.longitude
        this.wordScore = Number(cacheObject.wordScore)
        this.distance = 0
        this.score = 0
        return this
    }

}