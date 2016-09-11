import { expect } from 'chai'
import sinon from 'sinon'
import path from 'path'
import importManager from '../../src/data/importer'
import tsvHandler from '../../src/data/handlers/tsv'
import cityImporter from '../../src/data/importers/cities'

const sandbox = sinon.sandbox.create()

describe('Importer', () => {
  afterEach(() => {
    sandbox.restore()
  })

  it('has the correct interface', () => {
    expect(importManager.importFromFile).to.be.instanceof(Function)
  })

  describe('loadFile', () => {
    it('knows when to process a tsv file', (done) => {
      const testFile = {
        name: 'myFile.tsv',
        path: '/my/path/myFile.tsv'
      }

      const fileSpy = sandbox.stub(importManager, 'processFile').returns(Promise.resolve())

      importManager.loadFile(testFile)
        .then(() => {
          expect(fileSpy.calledOnce).to.be.true
          done()
        })
        .catch(done)
    })

    // @todo
    it.skip('knows when to process a zip file', (done) => {
      const testFile = {
        name: 'myFile.zip',
        path: '/my/path/myFile.zip'
      }

      const zipSpy = sandbox.stub(importManager, 'processZip').returns(Promise.resolve())

      importManager.loadFile(testFile)
        .then(() => {
          expect(zipSpy.calledOnce).to.be.true
          done()
        })
        .catch(done)
    })

    describe('processFile', () => {
      it('does processFile by correct handlers', (done) => {
        const testFile = {
          name: 'myFile.tsv',
          path: '/my/path/myFile.tsv'
        }

        const loadFileSpy = sandbox.stub(tsvHandler, 'loadFile').returns(Promise.resolve())

        importManager.processFile(testFile).then((result) => {
          expect(loadFileSpy.calledOnce).to.be.true
          done()
        }).catch(done)
      })
    })

    describe('preProcess', () => {
      it('does preprocess steps to all importers', (done) => {
        const preProcessSpy1 = sandbox.stub().returns(Promise.resolve(1))
        const preProcessSpy2 = sandbox.stub().returns(Promise.resolve(2))


        importManager.importers = [{
          preProcess: preProcessSpy1
        }, {
          preProcess: preProcessSpy2
        }]

        importManager.preProcess().then((result) => {
          expect(preProcessSpy1.calledOnce).to.be.true
          expect(preProcessSpy2.calledOnce).to.be.true
          expect(result).to.be.equal(2)
          done()
        }).catch(done)
      })
    })

    describe('importFromFile', () => {
      it('does import steps in order', (done) => {
        const loadFileSpy = sandbox.stub(importManager, 'loadFile').returns(Promise.resolve())
        const preProcessSpy = sandbox.stub(importManager, 'preProcess').returns(Promise.resolve())
        const doImportSpy = sandbox.stub(importManager, 'doImport').returns(Promise.resolve())

        importManager.importFromFile({}).then(() => {
          expect(loadFileSpy.calledOnce).to.be.true
          expect(preProcessSpy.calledOnce).to.be.true
          expect(doImportSpy.calledOnce).to.be.true
          sinon.assert.callOrder(loadFileSpy, preProcessSpy, doImportSpy)
          done()
        }).catch(done)
      })
    })
  })

  describe('Tsv Handler', () => {
    it('has the correct interface', () => {
      expect(tsvHandler.loadFile).to.be.instanceof(Function)
      expect(tsvHandler.extensions).to.contain('.tsv')
    })

    it('does convert a tsv file into a city object', (done) => {
      const file = [{
        path: path.resolve(`${__dirname}/../utils/fixtures/import-cities-1.tsv`),
        name: 'import-cities-1.tsv'
      }]

      tsvHandler.loadFile(file)
        .then((outputData) => {
          expect(outputData).to.be.instanceof(Array)
          expect(outputData).to.be.length(1)
          expect(outputData[0].file).to.be.instanceof(Object)
          expect(outputData[0].file.path).to.be.equal(path.resolve(`${__dirname}/../utils/fixtures/import-cities-1.tsv`))
          expect(outputData[0].file.name).to.be.equal('import-cities-1.tsv')
          expect(outputData[0].data).to.be.instanceof(Array)
          expect(outputData[0].data).to.be.length(10)
          expect(outputData[0].data[0]).to.be.length(19)
          done()
        })
        .catch(done)
    })
  })

  describe('City Importer', () => {
    it('has the correct interface', () => {
      expect(cityImporter.preProcess).to.be.instanceof(Function)
      expect(cityImporter.doImport).to.be.instanceof(Function)
    })

    it('does preprocess cities correctly from a tsv output', () => {
      const inputData = {
        tsv: [require('../utils/fixtures/import-cities-1.json')]
      }

      const outputData = cityImporter.preProcess(inputData)
      expect(outputData.tsv).to.be.instanceof(Array)
      expect(outputData.tsv[0].type).to.be.equal('cities')
      expect(outputData.tsv[0].data).to.be.instanceof(Array)
      expect(outputData.tsv[0].data[0]).to.be.instanceof(Object)
      expect(outputData.tsv[0].data[0]).to.have.all.keys(cityImporter.headers)
    })

    it('does correct calls to createCity', (done) => {
      const fCity = (i) => ({
        id: i,
        name: 'Abbotsford',
        ascii: 'Abbotsford',
        alt_name: 'Abbotsford,YXX,Абботсфорд',
        lat: '49.05798',
        long: '-122.25257',
        feat_class: 'P',
        feat_code: 'PPL',
        country: 'CA',
        cc2: '',
        admin1: '02',
        admin2: '5957659',
        admin3: '',
        admin4: '',
        population: '151683',
        elevation: '',
        dem: '114',
        tz: 'America/Vancouver',
        modified_at: '2013-04-22'
      })

      const ids = ['1', '2', '3']
      const cities = ids.map(fCity)

      const inputData = {
        tsv: [{
          type: 'cities',
          data: cities
        }]
      }

      const createCitySpy = sandbox.stub(cityImporter, 'createCity', (city) => city)

      cityImporter.doImport(inputData)
        .then((outputData) => {
          expect(outputData).to.be.an.instanceof(Array)
          expect(outputData).to.be.length(3)
          expect(outputData).to.be.deep.equal(cities)
          expect(createCitySpy.calledThrice).to.be.true
          done()
        })
        .catch(done)
    })
  })
})
