import { PrismaClient, Prisma } from '@prisma/client'
import { Response } from 'express'
import { SuggestionRequest } from './api/server'
const prisma = new PrismaClient()

type Result = {
    ascii: string
    fullName: string
    latitude: number
    longitude: number
    distance: number
    score: number
}

export const findCities = async (req: SuggestionRequest, res: Response) => {
    const { q, latitude, longitude } = req.query

    const findDistance = latitude && longitude ? true : false
    // latitude/longitude may be undefined, so check before verifying lat/long
    if (findDistance) {
        if (Math.abs(latitude!) > 90 || Math.abs(longitude!) > 90) {
            res.status(404).json({
                message:
                    'Invalid latitude or longitude value. Values must be between -90 and 90',
            })
            return
        }
    }
    // use unicode Normal Form Decomposition (NFD) to break down accented letters (é) to the base letter and the accent (e + ´), then replace those accents (found in range u0300 to u036f) with nothing ('')
    const cityNoAccents = q.normalize('NFD').replace(/[\u0300-\u036f]/g, '')

    // find distance between two globe coordinates, factoring in roundness. return number in km
    const distanceFormula = findDistance
        ? Prisma.sql`
             6371 * 2 * ASIN(
               SQRT(
                 POWER(SIN(RADIANS((${latitude}::float - latitude::float) / 2)), 2) +
                 COS(RADIANS(latitude::float)) * COS(RADIANS(${latitude}::float)) *
                 POWER(SIN(RADIANS((${longitude}::float - longitude::float) / 2)), 2)
               )
             ) 
             
             `
        : Prisma.empty

    // note that SIMILARITY is a function that compares strings and outputs a number, 1 for an identical match
    try {
        // prisma.sql enables conditional rendering in an SQL query and properly formats SQL syntax
        const suggestions: Result[] = await prisma.$queryRaw`
      SELECT
        name,
        "fullName",
        latitude,
        longitude,
        ${
            findDistance
                ? Prisma.sql`
                      PERCENT_RANK() OVER (ORDER BY( POWER(${distanceFormula},-1) 
                        * SIMILARITY("ascii", ${cityNoAccents})  ))`
                : Prisma.sql`SIMILARITY("ascii", ${cityNoAccents})`
        }
          AS score
      FROM  
        "City"
      WHERE 
          "ascii" ILIKE LOWER(${cityNoAccents})||'%' 
        AND 
          population >= 5000
      ORDER BY
        score DESC
      LIMIT
        5
    `
        if (suggestions.length > 0) {
            res.status(200).json({
                message: 'Retrieval successful',
                suggestions: suggestions,
            })
        } else {
            res.status(404).json({
                status: 404,
                message: `Database successfully queried, but no data matches the string "${q}"`,
                suggestions: suggestions,
            })
        }
    } catch (err) {
        console.log(err)
        res.status(500).json({ message: err })
    }
}
