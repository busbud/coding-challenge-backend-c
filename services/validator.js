exports.coordinateIsValid = (coordinate) => {
    return coordinate && !isNaN(parseFloat(coordinate)) && parseFloat(coordinate) <= 180 && parseFloat(coordinate) >= -180 ? true : false;
} 