export function calculateScore(wordScore: number, distance: number) {
    
    if(!distance || distance == 0)
        return Math.round((wordScore)*100)/100
    
    const distancePenalty = Math.floor(distance/(Number(process.env.DISTANCE_KM_FACTOR)*1000)) / Number(process.env.DISTANCE_PENALTY_FACTOR)
    return Math.round((wordScore - distancePenalty)*100)/100

}
