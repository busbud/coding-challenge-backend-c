module.exports = {
	calculateScore(suggestionList) {
		let score;
		const precision = 2;
		const distances = suggestionList.map((suggestion) => suggestion.dist);
		const populations = suggestionList.map((suggestion) => suggestion.population);
		const maxDistance = Math.max(...distances);
		const maxPopulation = Math.max(...populations);
		suggestionList.forEach((suggestion) => {
			let distanceScore = 0;
			if (maxDistance !== Infinity) {
				distanceScore = 1 - (suggestion.dist / maxDistance);
			}
			if (distanceScore === 0) {
				distanceScore = 0.1;
			}
			let populationScore = (suggestion.population / maxPopulation);
			if (populationScore === 1) {
				populationScore = 0.9;
			}
			score = (populationScore + distanceScore);

			score = this.round(score, precision);
			suggestion.score = score;
		});

		const result = suggestionList.map((suggestion) => {
			if (suggestion.score === 1) {
				suggestion.score = 0.9;
			} else if (suggestion.score < 0.1) {
				suggestion.score *= 10;
			}
			suggestion.score = this.round(suggestion.score, 1);

			return suggestion;
		});

		return result.sort((a, b) => b.score - a.score);
	},
	round(value, precision) {
		const scale = (10 ** precision);
		return Math.round(value * scale) / scale;
	},
};
