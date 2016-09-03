module.exports = {

    /**
     * findDistanceBetweenTwoCoords
     *
     * @params coords1 {Object}
     * @params coords1.lon {Float}
     * @params coords1.lat {Float}
     *
     * @params coords2 {Object}
     * @params coords2.lon {Float}
     * @params coords2.lat {Float}
     *
     * @return distance in km
     *
     * See : http://stackoverflow.com/questions/5260423/torad-javascript-function-throwing-error
     * */
    findDistanceBetweenTwoCoords : function(coords1, coords2) {
        var R = 6371; // Radius of the earth in km
        var dLat = (coords2.lat - coords1.lat) * Math.PI / 180;  // deg2rad below
        var dLon = (coords2.lon - coords1.lon) * Math.PI / 180;
        var a =
            0.5 - Math.cos(dLat)/2 +
            Math.cos(coords1.lat * Math.PI / 180) * Math.cos(coords2.lat * Math.PI / 180) *
            (1 - Math.cos(dLon))/2;

        return R * 2 * Math.asin(Math.sqrt(a));
    }
};