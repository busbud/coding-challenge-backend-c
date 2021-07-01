const chai = require('chai')
const chaiAsPromised = require('chai-as-promised')
const sinon = require('sinon')
const Mock = require('@elastic/elasticsearch-mock')
const { errors } = require('@elastic/elasticsearch')
const SearchEngineClient = require('../../../../src/services/searchEngine/SearchEngineClient')

chai.use(chaiAsPromised)

describe('SearchEngineClient', () => {
  let searchEngineClient = null

  describe('connect', () => {
    afterEach(function () {
      if (searchEngineClient) {
        return searchEngineClient.close()
      }
    })

    it('Should emit "connecting"', async function () {
      const mock = new Mock()
      // Add route for health check
      mock.add({
        method: 'GET',
        path: '/_cluster/health'
      }, () => {
        return { status: 'green' }
      })

      searchEngineClient = new SearchEngineClient({
        connection: mock.getConnection()
      })
      const connectingSpy = sinon.spy()
      searchEngineClient.on('connecting', connectingSpy)
      await searchEngineClient.connect()
      sinon.assert.calledOnce(connectingSpy)
    })

    it('Should emit "connected"', async function () {
      const mock = new Mock()
      // Add route for health check
      mock.add({
        method: 'GET',
        path: '/_cluster/health'
      }, () => {
        return { status: 'green' }
      })

      searchEngineClient = new SearchEngineClient({
        connection: mock.getConnection()
      })
      const connectedgSpy = sinon.spy()
      searchEngineClient.on('connected', connectedgSpy)
      await searchEngineClient.connect()
      sinon.assert.calledOnce(connectedgSpy)
    })

    it('Should emit "retry" when connection failed', async function () {
      this.timeout(5000)
      let connectionTry = 1
      const mock = new Mock()
      // Add route for health check
      mock.add({
        method: 'GET',
        path: '/_cluster/health'
      }, () => {
        if (connectionTry === 0) {
          return { status: 'green' }
        }
        connectionTry--
        return new errors.ResponseError({ body: {}, statusCode: 500 })
      })

      searchEngineClient = new SearchEngineClient({
        connection: mock.getConnection(),
        maxRetries: 1
      })
      const retrySpy = sinon.spy()
      searchEngineClient.on('retry', retrySpy)
      await searchEngineClient.connect()
      sinon.assert.calledOnce(retrySpy)
    })

    it('Should set "isConnected" property to true', async function () {
      const mock = new Mock()
      // Add route for health check
      mock.add({
        method: 'GET',
        path: '/_cluster/health'
      }, () => {
        return { status: 'green' }
      })

      searchEngineClient = new SearchEngineClient({
        connection: mock.getConnection()
      })
      await searchEngineClient.connect()
      chai.expect(searchEngineClient.isConnected).to.be.true // eslint-disable-line no-unused-expressions
    })

    it('Should throw an error after reach the max retries value',
      async function () {
        this.timeout(60000)
        const mock = new Mock()
        // Add route for health check
        mock.add({
          method: 'GET',
          path: '/_cluster/health'
        }, () => {
          return new errors.ResponseError({ body: {}, statusCode: 500 })
        })

        searchEngineClient = new SearchEngineClient({
          connection: mock.getConnection(),
          maxRetries: 1
        })

        await chai.expect(searchEngineClient.connect()).to.be.rejectedWith(Error)
      })

    it('Should throw an error if the client is already connected',
      async function () {
        const mock = new Mock()
        // Add route for health check
        mock.add({
          method: 'GET',
          path: '/_cluster/health'
        }, () => {
          return { status: 'green' }
        })

        searchEngineClient = new SearchEngineClient({
          connection: mock.getConnection()
        })
        await searchEngineClient.connect()
        await chai.expect(searchEngineClient.connect()).to.be.rejectedWith(Error)
      })
  })

  describe('close', () => {
    it('Should emit "closed"', async function () {
      const mock = new Mock()
      // Add route for health check
      mock.add({
        method: 'GET',
        path: '/_cluster/health'
      }, () => {
        return { status: 'green' }
      })

      searchEngineClient = new SearchEngineClient({
        connection: mock.getConnection()
      })
      await searchEngineClient.connect()

      const closedSpy = sinon.spy()
      searchEngineClient.on('closed', closedSpy)
      await searchEngineClient.close()
      sinon.assert.calledOnce(closedSpy)
    })

    it('Should set "isConnected" property to false', async function () {
      const mock = new Mock()
      // Add route for health check
      mock.add({
        method: 'GET',
        path: '/_cluster/health'
      }, () => {
        return { status: 'green' }
      })

      searchEngineClient = new SearchEngineClient({
        connection: mock.getConnection()
      })
      await searchEngineClient.connect()
      await searchEngineClient.close()
      chai.expect(searchEngineClient.isConnected).to.be.false // eslint-disable-line no-unused-expressions
    })
  })

  describe('healthCheck', () => {
    it('Should returns a reponse with "clusterName" and "status" properties',
      async function () {
        const mock = new Mock()
        // Add route for health check
        mock.add({
          method: 'GET',
          path: '/_cluster/health'
        }, () => {
          return { status: 'green' }
        })

        searchEngineClient = new SearchEngineClient({
          connection: mock.getConnection()
        })
        await searchEngineClient.connect()
        const response = await searchEngineClient.healthCheck()
        chai.expect(response).to.have.keys(['clusterName', 'status'])
      })
  })

  describe('addRepository', () => {
    it('Should set the repository', async function () {
      const mock = new Mock()
      // Add route for health check
      mock.add({
        method: 'GET',
        path: '/_cluster/health'
      }, () => {
        return { status: 'green' }
      })

      searchEngineClient = new SearchEngineClient({
        connection: mock.getConnection()
      })
      await searchEngineClient.connect()
      searchEngineClient.addRepository('test', () => {})
      chai.expect(searchEngineClient.repositories.has('test')).to.be.true // eslint-disable-line no-unused-expressions
    })

    it('Should throw an error if the client is not connected',
      async function () {
        const mock = new Mock()
        // Add route for health check
        mock.add({
          method: 'GET',
          path: '/_cluster/health'
        }, () => {
          return { status: 'green' }
        })

        searchEngineClient = new SearchEngineClient({
          connection: mock.getConnection()
        })

        chai.expect(() =>
          searchEngineClient.addRepository('test', () => {})).to.throw()
      })
  })

  describe('getRepository', () => {
    it('Should get the repository', async function () {
      const mock = new Mock()
      // Add route for health check
      mock.add({
        method: 'GET',
        path: '/_cluster/health'
      }, () => {
        return { status: 'green' }
      })

      searchEngineClient = new SearchEngineClient({
        connection: mock.getConnection()
      })
      await searchEngineClient.connect()

      const repository = {
        test () {}
      }

      const repositoryBuilder = () => {
        return repository
      }

      searchEngineClient.addRepository('test', repositoryBuilder)
      chai.expect(searchEngineClient.getRepository('test'))
        .to
        .be
        .equal(repository)
    })

    it('Should throw an error if the client is not connected',
      async function () {
        const mock = new Mock()
        // Add route for health check
        mock.add({
          method: 'GET',
          path: '/_cluster/health'
        }, () => {
          return { status: 'green' }
        })

        searchEngineClient = new SearchEngineClient({
          connection: mock.getConnection()
        })

        chai.expect(() =>
          searchEngineClient.getRepository('test', () => {})).to.throw()
      })
  })
})
