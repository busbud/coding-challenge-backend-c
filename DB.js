"use strict";

class DB {
    data
    
    constructor(data){
        this.data = data
    }

   
    search(query){
        const response = []
        for(let row of this.data){
            const searchResult = this.score.search(query,row)


            if(searchResult && searchResult.stringScore){
                const {stringScore,distance} = searchResult
                response.push({
                    score:stringScore,
                    name: `${row.name}, ${row.admin1}, ${row.country}`,
                    latitude: row.lat,
                    longitude: row.long,
                    distance
                })
            }
        }
        

        return response.sort((a,b) =>  {
            const n = b.score - a.score

            if(n!== 0){
                return n
            }

            return a.distance - b.distance
        } )
    
    }


    
}


DB.prototype.score = undefined
module.exports = DB