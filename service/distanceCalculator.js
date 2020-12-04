module.exports = {
	distanceCalculator: (userLongitude, userLatitude, cityLongitude, cityLatitude) => {
		if ((userLongitude === cityLongitude) && (userLatitude === cityLatitude)) {
			return 0;
		}
		const deg2rad = (degree) => (Math.PI * degree) / 180;
		const rad2deg = (rad) => (rad * 180.0) / Math.PI;

		const theta = userLongitude - cityLongitude;
		let dist = Math.sin(deg2rad(userLatitude)) * Math.sin(deg2rad(cityLatitude))
            + Math.cos(deg2rad(userLatitude)) * Math.cos(deg2rad(cityLatitude)) * Math.cos(deg2rad(theta));

		dist = Math.acos(dist);
		dist = rad2deg(dist);
		dist = dist * 60 * 1.1515;
		return dist * 1.609344;
	},
};
