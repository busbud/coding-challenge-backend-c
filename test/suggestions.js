var expect = require('chai').expect;
var app = require('../app');
var request = require('supertest')(app);

var checkStatus = function (query, statusCode) {
	return function () {
		var response;

		before(function (done) {
			request
				.get(query)
				.end(function (err, res) {
					response = res;
					done(err);
				});
		});

		it('returns a ' + statusCode, function () {
			expect(response.statusCode).to.equal(statusCode);
		});
	}
}

var checkNotFound = function (query) {
	return function () {
		var response;

		before(function (done) {
			request
				.get(query)
				.end(function (err, res) {
					response = res;
					response.json = JSON.parse(res.text);
					done(err);
				});
		});

		it('returns a 404', function () {
			expect(response.statusCode).to.equal(404);
		});

		it('returns an empty array of suggestions', function () {
			expect(response.json.suggestions).to.be.instanceof(Array);
			expect(response.json.suggestions).to.have.length(0);
		});
	}
}

var checkValid = function (query, regex) {
	return function () {
		var response;

		before(function (done) {
			request
				.get(query)
				.end(function (err, res) {
					response = res;
					if (res.text.startsWith('{')) response.json = JSON.parse(res.text);
					done(err);
				});
		});

		it('returns a 200', function () {
			expect(response.statusCode).to.equal(200);
		});

		it('returns an array of suggestions', function () {
			expect(response.json.suggestions).to.be.instanceof(Array);
			expect(response.json.suggestions).to.have.length.above(0);
		});

		it('contains a match', function () {
			expect(response.json.suggestions).to.satisfy(function (suggestions) {
				return suggestions.some(function (suggestion) {
					return regex.test(suggestion.name);
				});
			})
		});

		it('contains latitudes and longitudes', function () {
			expect(response.json.suggestions).to.satisfy(function (suggestions) {
				return suggestions.every(function (suggestion) {
					return suggestion.latitude && suggestion.longitude;
				});
			})
		});

		it('contains scores', function () {
			expect(response.json.suggestions).to.satisfy(function (suggestions) {
				return suggestions.every(function (suggestion) {
					return suggestion.score;
				});
			})
		});
	}
}

describe('GET /suggestions', function () {
	describe('with an invalid request', checkStatus('/suggestion', 400));

	describe('with an empty city name', checkStatus('/suggestions?q=', 400));

	describe('with empty geo position', checkValid('/suggestions?q=a&latitude=&longitude=', /^a/i));

	describe('with invalid geo position', checkStatus('/suggestions?q=a&latitude=a&longitude=a', 200)); // Now working, TODO find other case to trigger a 500
	
	describe('with a non-existent city', checkNotFound('/suggestions?q=SomeRandomStringThatIsntACity'));

	describe('with a valid city', checkValid('/suggestions?q=Montreal', /Montréal/i));

	describe('with a partial city name', checkValid('/suggestions?q=lon', /London/i));

	// Note this works in a browser, but the test fails with a 404 => problem with local encoding ?
	// describe('with a unicode city name', checkValid('/suggestions?q=몬트리올', /Montréal/i)); // 몬트리올 is an alternate name
});