const expect = require('chai').expect;
import PrefixTree from '../../data/prefix-tree';

describe('Prefix tree', () => {
	let trie = null;
	const data = {
		x: 1,
		y: 2,
		z: 'funny'
	};

	beforeEach(() => {
		trie = new PrefixTree();
	});

	describe('#add', () => {
		it('should add single letter word', () => {
			trie.add('A', data);
			expect(trie.heads).to.deep.equal({
				A: {
					data
				},
			});
		});

		it('should add 2 letters word', () => {
			trie.add('ab', data);
			expect(trie.heads).to.deep.equal({
				a: {
					b: {
						data
					}
				},
			});
		});

		it('should be able to add multiple words', () => {
			trie.add('abcd', data);
			trie.add('abc', data);
			trie.add('ab', data);
			trie.add('abxy', data);
			trie.add('a', data);

			expect(trie.heads).to.deep.equal({
				a: {
					b: {
						c: {
							d: {
								data
							},
							data,
						},
						x: {
							y: {
								data
							}
						},
						data,
					},
					data,
				},
			});
		});
	});

	describe('#findMatches', () => {
		let trie = null;
		const data = {
			x: 1,
			y: 2,
			z: 'funny'
		};

		beforeEach(() => {
			trie = new PrefixTree();
			trie.add('abcd', data);
			trie.add('abc', data);
			trie.add('ab', data);
			trie.add('abxy', data);
			trie.add('a', data);
		});

		it('should find all matches for single letter word', () => {

			const result = trie.findMatches('a');
			expect(result).to.deep.equal({
				'a': data,
				'ab': data,
				'abc': data,
				'abcd': data,
				'abxy': data,
			});
		});

		it('should return multiple results when search word is partial', () => {
			const result = trie.findMatches('abc');
			expect(result).to.deep.equal({
				'abc': data,
				'abcd': data,
			});
		});

		it('should return exact match if complete search word is provided', () => {
			let result = trie.findMatches('abcd');
			expect(result).to.deep.equal({
				'abcd': data,
			});
			result = trie.findMatches('abxy');
			expect(result).to.deep.equal({
				'abxy': data,
			});
		});

		it('should not return any match if partial search word does not match any prefix', () => {
			const result = trie.findMatches('abcde');
			expect(result).to.deep.equal({});
		});
	});
});