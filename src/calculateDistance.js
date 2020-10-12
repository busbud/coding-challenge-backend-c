module.exports = function (latitude1, longitude1, latitude2, longitude2) {
	if ((latitude1 == latitude2) && (longitude1 == longitude2)) {
		return 0;
	}
	else {
		let radLatitude1 = Math.PI * latitude1/180;
		let radLatitude2 = Math.PI * latitude2/180;
		let theta = longitude1 - longitude2; 
		let radtheta = Math.PI * theta/180;
		var dist = Math.sin(radLatitude1) * Math.sin(radLatitude2) + Math.cos(radLatitude1) * Math.cos(radLatitude2) * Math.cos(radtheta);
		if (dist > 1) {
			dist = 1;
		}
		dist = Math.acos(dist);
		dist = dist * 180/Math.PI;
		dist = dist * 60 * 1.1515;
		return dist;
	}
}
