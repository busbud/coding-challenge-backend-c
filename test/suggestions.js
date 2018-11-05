
const _ = require('lodash')
const ava = require('ava')
const request = require('request')

ava.serial.cb('GET /suggestions - with a non-existent city', test => {

	request.get(
		{
			url: 'http://127.0.0.1:3456/suggestions?q=kwfh58J7IQafNeFw',
			json: true
		},
		(error, response, body) => {

			test.is(!error, true)
			test.is(response.statusCode, 200)
			test.is(_.isArray(body.suggestions), true)
			test.is(_.isEmpty(body.suggestions), true)

			test.end()

		})

})

ava.serial.cb('GET /suggestions - with a valid city', test => {

	request.get(
		{
			url: 'http://127.0.0.1:3456/suggestions?q=Lon',
			json: true
		},
		(error, response, body) => {

			test.is(!error, true)
			test.is(response.statusCode, 200)
			test.is(_.isArray(body.suggestions), true)
			test.is(!_.isEmpty(body.suggestions), true)
			test.is(body.suggestions[0].name.indexOf('London, '), 0)

			test.end()

		})

})

ava.serial.cb('GET /suggestions - with misspelling', test => {

	request.get(
		{
			url: 'http://127.0.0.1:3456/suggestions?q=Lndon',
			json: true
		},
		(error, response, body) => {

			test.is(!error, true)
			test.is(response.statusCode, 200)
			test.is(_.isArray(body.suggestions), true)
			test.is(!_.isEmpty(body.suggestions), true)
			test.is(body.suggestions[0].name.indexOf('London, '), 0)

			test.end()

		})

})


