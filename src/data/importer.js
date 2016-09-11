import path from 'path'
import tsvHandler from './handlers/tsv'
import citiesImporter from './importers/cities'

export const importManager = () => {
  const series = (list, args) => {
    return list.reduce((chain, fn) => {
      chain = chain.then(fn)
      return chain
    }, Promise.resolve(args))
  }

  const isZip = () => {
    return false
  }

  return {
    handlers: [ tsvHandler ],
    importers: [ citiesImporter ],

    /**
     * The main method of the ImportManager
     * execute all method to extract data from file !
     * @param {File} file
     * @returns {Promise}
     */
    importFromFile(file) {
      return this.loadFile(file)
        .then((data) => this.preProcess(data))
        .then((importData) => {
          return this.doImport(importData)
        })
    },

    /**
     * Load the given file into usable importData in the format: { data: {} }, regardless of
     * the file is a single importable file, or a zip file containing loads of files.
     * @param {File} file
     * @returns {Promise}
     */
    loadFile(file) {
      return isZip(file) ? this.processZip(file) : this.processFile(file)
    },

    /**
     * Pass the prepared importData through the preProcess function of the various importers, so that the importers can
     * make any adjustments to the data based on relationships between it
     * @param {ImportData} importData
     * @returns {Promise(ImportData)}
     */
    preProcess(importData = {}) {
      const opts = this.importers.map((i) => i.preProcess.bind(i))
      return series(opts, importData)
    },

    /**
     * Each importer gets passed the data from importData which has the key matching its type. it only gets the
     * data that it should import. Each importer then handles actually importing that data
     * @param {ImportData} importData
     * @returns {Promise(ImportData)}
     */
    doImport(importData = {}) {
      const opts = this.importers.map((i) => i.doImport(importData))
      return Promise.all(opts)
    },

    /**
     * Process File
     * Takes a reference to a single file, sends it to the relevant handler to be loaded and returns an object in the
     * importData format: { data: {} }
     * The data key contains JSON representing any data that should be imported
     * @param {File} file
     * @returns {Promise(ImportData)}
     */
    processFile(file) {
      const ext = path.extname(file.name).toLowerCase()
      const fHandler = this.handlers.find((handler) => {
        return handler.extensions.includes(ext)
      })

      if (!fHandler) {
        return Promise.reject(new Error(`no handler for ${ext}`))
      }

      const format = (data) => {
        return {
          [fHandler.type]: data
        }
      }

      return fHandler.loadFile([ file ])
        .then(format)
    },

    /**
     * @todo
     * Takes a reference to a zip file, extracts it, sends files from inside to the right handler, and
     * returns an object in the importData format: { data: {} }
     * The data key contains JSON representing any data that should be imported
     * @param {File} file
     * @returns {Promise(ImportData)}
     */
    processZip(file) {
    }
  }
}

export default importManager()
