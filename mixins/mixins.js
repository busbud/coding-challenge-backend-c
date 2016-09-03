module.exports = {

    /**
     * findDistanceBetweenTwoCoords
     *
     * @See http://stackoverflow.com/questions/5260423/torad-javascript-function-throwing-error
     *
     * @param {Object} coords1      First coordinates
     * @param {float} coords1.lon   Longitude
     * @param {float} coords1.lat   Latitude
     *
     * @param {Object} coords2      Second coordinates
     * @param {float} coords2.lon   Longitude
     * @param {float} coords2.lat   Latitude
     *
     * @return {float} distance in meter
     *
     * */
    findDistanceBetweenTwoCoords : function(coords1, coords2) {
        var R = 6371000; // Radius of the earth in m
        var dLat = (coords2.lat - coords1.lat) * Math.PI / 180;  // deg2rad below
        var dLon = (coords2.lon - coords1.lon) * Math.PI / 180;
        var a =
            0.5 - Math.cos(dLat)/2 +
            Math.cos(coords1.lat * Math.PI / 180) * Math.cos(coords2.lat * Math.PI / 180) *
            (1 - Math.cos(dLon))/2;
        var distance = R * 2 * Math.asin(Math.sqrt(a));

        return distance;
    }
};