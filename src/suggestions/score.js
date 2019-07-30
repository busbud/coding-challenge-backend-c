/**
 * To improve -> look for mathematical operations that might end
 * up making the query less costly.
 * 
 * @param {number} weight 
 * @returns {object}
 */
const getPopulationScoreExpr = (weight) => {
    return {
      $switch: {
        branches: [        
          {
            case: { $gte : [ "$population", 1000000 ] },
            then: 1 * weight
          },
          {
            case: { $gte : [ "$population", 500000 ] },
            then: 0.8 * weight
          },
          {
            case: { $gte : [ "$population", 250000 ] },
            then: 0.6 * weight
          },
          {
            case: { $gte : [ "$population", 125000 ] },
            then: 0.4 * weight
          },
          {
            case: { $gte : [ "$population", 75000 ] },
            then: 0.2 * weight
          }        
        ],
        default: 0
      }
    }
  }

  /**
   * 
   * @param {number} weight 
   * @returns {object}
   */
  const getDistanceScoreExpr = (weight) => {
    return {
      $switch: {
        branches: [        
          {
            case: { $lte : [ "$distance", 100 * 1000] },
            then: 1 * weight
          },
          {
            case: { $lte : [ "$distance", 200 * 1000 ] },
            then: 0.8 * weight
          },
          {
            case: { $lte : [ "$distance", 300 * 1000 ] },
            then: 0.6 * weight
          },
          {
            case: { $lte : [ "$distance", 400 * 1000 ] },
            then: 0.4 * weight
          },
          {
            case: { $lte : [ "$distance", 500 * 1000 ] },
            then: 0.2 * weight
          }        
        ],
        default: 0
      }
    }
  }

  /**
   * 
   * @param {string} q 
   * @param {number} weight 
   * @returns {object}
   */
  const getNameScoreExpr = (q, weight) => {
  
    const reverseScore = {
      $divide: [ 
      { $indexOfBytes: [ "$searchField", q ] },
      { $strLenBytes: "$searchField" }
    ]};  
    
    const score = {$subtract: [1, reverseScore]}
    
    const weightScore = { $multiply: [ score, weight ] }
    
    return weightScore;    
  }

  /**
   * 
   * @param {obect} requestParamns 
   * @returns {object}
   */
  const getScoreQuery = (requestParamns) => {
  
    const scoreFunctions = [
      getNameScoreExpr(requestParamns.q, 0.4),
      getPopulationScoreExpr(0.3)
    ];
    
    // It doesn't matter if max score is 1 or 0.6 since it's a relative value.
    if( requestParamns.long && requestParamns.lat) {
      scoreFunctions.push(getDistanceScoreExpr(0.3))
    }
    return {    
        $add: scoreFunctions
    }
  }

  module.exports.getScoreQuery = getScoreQuery;
