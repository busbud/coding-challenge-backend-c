<?php
describe('GET /suggestions', function() {
	$this->curl = new \Curl\Curl();
	$this->url = 'http://localhost:8000';

	describe('with a non-existent city', function () {
		beforeEach(function () {
			$this->curl->get($this->url . '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere');
			$this->json = $this->curl->isCurlError() ? null : json_decode($this->curl->getRawResponse(), true);
		});
		it('returns a 404', function () {
			expect($this->curl->getHttpStatusCode())->toBe(404);
		});
		it('returns an empty array of suggestions', function () {
			expect($this->json['suggestions'])->toBeA('array');
			expect($this->json['suggestions'])->toHaveLength(0);
		});
	});

	describe('with a valid city', function () {
		$this->search = 'lond';
		beforeEach(function () {
			$this->curl->get($this->url . '/suggestions?q=' . $this->search);
			$this->json = $this->curl->isCurlError() ? null : json_decode($this->curl->getRawResponse(), true);
		});
		it('returns a 200', function () {
			expect($this->curl->getHttpStatusCode())->toBe(200);
		});

		it('returns an array of suggestions', function () {
			expect($this->json['suggestions'])->toBeA('array');
			expect(count($this->json['suggestions']))->toBeGreaterThan(0);
		});

		describe('Validate the shape of the data being returned', function() {
			it('contains latitudes and longitudes', function () {
				// NOTE : Kahlan does not have an every() method
				foreach($this->json['suggestions'] as $suggestion){
					expect($suggestion)->toContainKey('latitude');
					expect($suggestion)->toContainKey('longitude');
					expect(floatval($suggestion['latitude']) == $suggestion['latitude'])->toBe(true);
					expect(floatval($suggestion['longitude']) == $suggestion['longitude'])->toBe(true);
				}
			});

			it('contains scores', function () {
				foreach($this->json['suggestions'] as $suggestion){
					expect($suggestion)->toContainKey('score');
					expect(floatval($suggestion['score']) == $suggestion['score'])->toBe(true);
					expect(0 <= $suggestion['score'])->toBe(true);
					expect($suggestion['score'] <= 1)->toBe(true);
				}
			});
		});
		it('contains a match', function () {
			$pattern = '#' . $this->search . '#i';
			// NOTE : Kahlan does not have a some() method
			$matched = false;
			foreach($this->json['suggestions'] as $suggestion){
				expect($suggestion)->toContainKey('name');
				if(preg_match($pattern, $suggestion['name']))
				{
					$matched = true;
				}
			}
			if(!$matched)
			{
				expect($matched)->toBe(true);
			}
		});
		it('contains scores in descending order', function () {
			$lastscore = 1.0;
			foreach($this->json['suggestions'] as $suggestion){
				expect($suggestion['score'] <= $lastscore)->toBe(true);
				$lastscore = $suggestion['score'];
			}
		});
	});
});
