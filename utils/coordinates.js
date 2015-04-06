module.exports = {
    
    // lets be very verbose to make sure every condition is very clear
    validCoordinate: function(coordinate) {
        
        // no given coordinate is considered 'valid'
        if (!coordinate.latitude && !coordinate.longitude) {
            return true;
        }
        
        if (!isNaN(coordinate.latitude) && !isNaN(coordinate.longitude)) {
            if (Math.abs(coordinate.latitude) > 90 || Math.abs(coordinate.longitude) > 180) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }
}