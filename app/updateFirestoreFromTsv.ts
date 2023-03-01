import { kernel } from './Kernel'
import { Logger } from 'pino'
import { TYPES } from './utils/Types'
import { Firestore } from '@google-cloud/firestore'
import { parse } from 'csv-parse'
import * as fs from 'fs'
import * as path from 'path'
import { DataHeadersTsv } from './entities/DataHeadersTsv'
import { City } from './entities/City'

const logger = kernel.get<Logger>(TYPES.Logger)

const firestoreDb = kernel.get<Firestore>(TYPES.Firestore)
const citiesCollection = firestoreDb.collection('cities')
async function deleteData(): Promise<void> {
    const documents = await citiesCollection.listDocuments()
    documents.map(async (doc) => {
        await doc.delete()
    })
}
async function addData(): Promise<void> {
    /* eslint-disable camelcase */
    const csvFilePath = path.resolve('data/cities_canada-usa.tsv')
    const fileContent = fs.readFileSync(csvFilePath, { encoding: 'utf-8' })

    // Initialize the parser
    parse(
        fileContent,
        {
            delimiter: '\t',
            relax_quotes: true,
            columns: DataHeadersTsv,
            fromLine: 2,
            on_record: async (line): Promise<City> => {
                const firestoreDoc = {
                    id: line.id,
                    name: line.name,
                    longitude: Number(line.long),
                    latitude: Number(line.lat),
                    population: Number(line.population),
                    country: line.country
                }
                await citiesCollection.add(firestoreDoc)
                return {
                    id: line.id,
                    name: line.name,
                    longitude: Number(line.long),
                    latitude: Number(line.lat),
                    population: Number(line.population),
                    country: line.country
                }
            }
        },
        (err, data) => {
            if (err) {
                logger.error(err.message, {
                    tags: ['data', 'update'],
                    stack: err.stack
                })
            } else {
                logger.info(`Total records refreshed: ${data.length}`, {
                    tags: ['data', 'update', 'success']
                })
            }
        }
    )
}

async function refreshData(): Promise<void> {
    await deleteData()
    await addData()
}

refreshData().then(() => {
    logger.info('Records refreshed successfully', {
        tags: ['data', 'update', 'success']
    })
})
