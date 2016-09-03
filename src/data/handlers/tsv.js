import fs from 'fs'
import parse from 'csv-parse'

export default {
  type: 'tsv',
  extensions: ['.tsv'],
  parser: {
    delimiter: '\t',
    quote: '""""',
    escape: '""""'
  },

  parseFile(file) {
    return new Promise((resolve, reject) => {
      const lines = []
      const parser = parse(this.parser)

      const fsReadStream = fs.createReadStream(file.path, {
        encoding: 'utf-8'
      })

      fsReadStream.pipe(parser)

      parser.on('readable', () => {
        let chunk
        while ((chunk = parser.read()) !== null) {
          lines.push(chunk)
        }
      })

      parser.on('error', reject)
      parser.on('finish', () => {
        resolve({
          file,
          data: lines
        })
      })
    })
  },

  loadFile(files) {
    const ops = files.map((f) => this.parseFile(f))
    return Promise.all(ops)
  }
}
