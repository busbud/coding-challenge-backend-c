const provinces = {
	1: 'AB',
	2: 'BC',
	3: 'MB',
	4: 'NB',
	5: 'NL',
	6: 'NA',
	7: 'NS',
	8: 'ON',
	9: 'PE',
	10: 'QC',
	11: 'SK'
}
module.exports.provinceFromId = function(id) {
	const parsedId = parseInt(id);
	if(parsedId) {
		return provinces[parsedId];
	} else {
		return null;
	}
}
