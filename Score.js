// Earth
const  radius = 6371


class Score {

    search({
        q,
        latitude,
        longitude
    }, {
        name,
        lat,
        long
    }) {

        const stringScore = this.stringScore(q,name)
        if(latitude && longitude && stringScore > 0){
            return {
                stringScore,
                distance: this.getDirectDistance(latitude,longitude,lat,long)
            }
        }

        return { 
            stringScore
        }
        


    }

    stringScore(query='', string='') {
        const queryLower =  query.toLowerCase()
        const stringLower = string.toLowerCase()

        if (query === string) return 1

        if (query === '') return 0

        if(stringLower.indexOf(queryLower) == -1) return 0

        

        const queryLen = query.length
        const stringLen = string.length

        let mutual = ''
        let longest = ''
        
        for(let letterIndex in queryLower) {
            
            if(queryLower[letterIndex] === stringLower[letterIndex]){
                mutual += queryLower[letterIndex]
                longest = longest.length > mutual ? longest : mutual
            }else{
                mutual = ''
            }
        }
        const score = longest.length / stringLen

        return parseFloat(score.toFixed(2))

    }


    // Havelsine
    // https://github.com/CanDgrmc/havelsine
    getDirectDistance(lat1, lng1, lat2, lng2) {
        const deltaLat = this.degrees_to_radians(lat2 - lat1)
        const deltaLng = this.degrees_to_radians(lng2 - lng1)
        const sindLat = Math.sin(deltaLat / 2)
        const sindLng = Math.sin(deltaLng / 2);

        const alpha = Math.pow(sindLat, 2) + Math.pow(sindLng, 2) * Math.cos(this.degrees_to_radians(lat1)) * Math.cos(this.degrees_to_radians(lat2));
        // degrees between 2 points
        const theta = 2 * Math.atan2(Math.sqrt(alpha), Math.sqrt(1 - alpha))
        const dist = radius * theta;

        return dist

    }

    degrees_to_radians(degrees) {
        const pi = Math.PI;
        return degrees * (pi / 180);
    }
}

module.exports = Score;