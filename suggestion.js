const stringSimilarity = require('string-similarity')
const Geopoint = require('geopoint')
const accents = require('remove-accents')

const dataParser = require('./data_parser')

const allCities = dataParser.generateData()

const generate = (params) => {
    const factors = []
    const results = {}
    const output = []

    if (params) {
        const useDistance = validateLatLong(params)
        filterCities(params, factors, results, useDistance)
        calculateScores(factors, results, useDistance)
        formatResults(results, output)
    }
    return output
}

const filterCities = (params, factors, results, useDistance) => {
    if (!('q' in params) || params.q === '') {
        return
    }
    const qLower = accents.remove(params.q.toLowerCase())

    allCities.forEach((city) => {
        const cityLower = accents.remove(city.name.toLowerCase())
        if (cityLower.startsWith(qLower)) {
            const formattedName = formatName(city)
            const current = { name: formattedName, similarity: getSimilarity(qLower, cityLower) }
            if (useDistance) {
                current.distance = getDistance(params.latitude, params.longitude, city.lat, city.long)
            } else {
                current.population = city.population
            }
            factors.push(current)
            results[formattedName] = {
                name: formattedName,
                latitude: city.lat,
                longitude: city.long,
                score: 0
            }
        }
    })
}

const validateLatLong = (params) => {
    let valid = true
    if (!('latitude' in params) || !('longitude' in params)) {
        valid = false
    } else if (params.latitude <= -90 ||
        params.latitude >= 90 ||
        params.longitude <= -180 ||
        params.longitude >= 180) {
        valid = false
    }
    return valid
}

const formatName = (city) => {
    return `${city.name}, ${city.admin1}, ${city.country}`
}

const getSimilarity = (s1, s2) => {
    return stringSimilarity.compareTwoStrings(accents.remove(s1), accents.remove(s2))
}

const getDistance = (lat1, long1, lat2, long2) => {
    const p1 = new Geopoint(parseFloat(lat1), parseFloat(long1))
    const p2 = new Geopoint(parseFloat(lat2), parseFloat(long2))
    return p1.distanceTo(p2, true)
}

const calculateScores = (factors, results, useDistance) => {
    if (!Object.keys(results).length) {
        return
    }
    if (useDistance) {
        calculateWithDistance(factors, results)
    } else {
        calculateWithPopulation(factors, results)
    }
}

const calculateWithDistance = (factors, results) => {
    factors.sort((a, b) => parseFloat(a.distance) - parseFloat(b.distance))
    const lowestDist = factors[0].distance
    factors.forEach((city) => {
        const distScore = lowestDist / city.distance
        const score = city.similarity * 0.7 + distScore * 0.3
        results[city.name].score = score.toFixed(2)
    })
}

const calculateWithPopulation = (factors, results) => {
    factors.sort((a, b) => parseFloat(b.population) - parseFloat(a.population))
    const highestPop = factors[0].population
    factors.forEach((city) => {
        const popScore = city.population / highestPop
        const score = city.similarity * 0.7 + popScore * 0.3
        results[city.name].score = score.toFixed(2)
    })
}

const formatResults = (results, output) => {
    for (const city in results) {
        output.push(results[city])
    }
    output.sort((a, b) => parseFloat(b.score) - parseFloat(a.score))
}

module.exports = { generate }
