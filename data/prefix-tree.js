export default class PrefixTree {
	constructor() {
		// Key-value pairs
		// key: char
		// value: children
		this.heads = {};
	}

	add(str, data) {
		this._add(this.heads, this._getCharacters(str), data);
	}

	_add(childNodes, remainingChars, data) {
		if (remainingChars.length === 1) {
			const lastChar = remainingChars[0];
			if (!childNodes[lastChar]) {
				childNodes[lastChar] = {};
			}
			childNodes[lastChar].data = data;
			return;
		}

		const [nextChar, ...nextRemainingChars] = remainingChars;
		if (!childNodes[nextChar]) {
			childNodes[nextChar] = {};
		}

		this._add(childNodes[nextChar], nextRemainingChars, data);
	}

	findMatches(str) {
		const matchedTree = this._findRecursive(this.heads, this._getCharacters(str));
		return this._constructMatchResults(matchedTree, str);
	}

	_findRecursive(childNodes, remainingChars) {
		if (remainingChars.length === 1) {
			const lastChar = remainingChars[0];
			return childNodes[lastChar] ? childNodes[lastChar] : {};
		}

		const [nextChar, ...nextRemainingChars] = remainingChars;
		return childNodes[nextChar] ? this._findRecursive(childNodes[nextChar], nextRemainingChars) : {};
	}

	_constructMatchResults(matchedTree, str) {
		return this._constructMatchResultsRecursive(matchedTree, str);
	}

	_constructMatchResultsRecursive(matchedTree, str) {
		const results = {};
		for (let key in matchedTree) {
			if (key === 'data') {
				results[str] = matchedTree.data;
			} else {
				const childResults = this._constructMatchResultsRecursive(matchedTree[key], str+key);
				Object.assign(results, childResults);
			}
		}

		return results;
	}

	// For strings with multi-byte unicode characters, length != number of chars.
	_getCharacters(str) {
		// Using ES6 spread operator breaks the string actual character-wise
		// instead of byte wise.
		// https://mathiasbynens.be/notes/javascript-unicode
		return [...str];
	}
}