import { PrismaClient, City } from '@prisma/client'
import fs from 'fs'
import csvParser from 'csv-parser'
import { provinceCodes } from '../data/codes'
const prisma = new PrismaClient()

type TSVRow = {
    name: string
    country: string
    id: string
    admin1: string
    ascii: string
    lat: string
    long: string
    population: string
}

const seed = async () => {
    try {
        fs.createReadStream('data/cities_canada-usa.tsv')
            .pipe(csvParser({ separator: '\t' }))
            .on('data', async (row: TSVRow) => {
                const countryStr = row.country === 'CA' ? 'Canada' : 'USA'
                const fullName = row.admin1
                    ? `${row.name}, ${
                          provinceCodes[row?.country][row?.admin1]
                      }, ${countryStr}`
                    : row.name
                const cityData: City = {
                    id: row.id,
                    name: row.name,
                    ascii: row.ascii,
                    fullName: fullName,
                    latitude: Number(row.lat),
                    longitude: Number(row.long),
                    country: row.country,
                    population: Number(row.population),
                }
                await prisma.city.create({
                    data: cityData,
                })
            })
    } catch (error) {
        console.error('Error seeding database:', error)
    } finally {
        await prisma.$disconnect()
    }
}

seed()
