exports.cities = function(a, b) {
	if (a.city < b.city) {
		return -1;
	} else if (a.city > b.city) {
		return 1;
	} else {
		return 0;
	}
};

exports.scores = function(a, b) {
	if (a.score === b.score) {
		return (a.name < b.name) ? -1 : (a.name > b.name) ? 1 : 0;
	} else {
		return (a.score > b.score) ? -1 : 1;
	}
};