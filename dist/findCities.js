"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.findCities = void 0;
const client_1 = require("@prisma/client");
const prisma = new client_1.PrismaClient();
const findCities = (req, res) => __awaiter(void 0, void 0, void 0, function* () {
    const { q, latitude, longitude } = req.query;
    const findDistance = latitude && longitude ? true : false;
    // latitude/longitude may be undefined, so check before verifying lat/long
    if (findDistance) {
        if (Math.abs(latitude) > 90 || Math.abs(longitude) > 90) {
            res.status(404).json({
                message: 'Invalid latitude or longitude value. Values must be between -90 and 90',
            });
            return;
        }
    }
    // use unicode Normal Form Decomposition (NFD) to break down accented letters (é) to the base letter and the accent (e + ´), then replace those accents (found in range u0300 to u036f) with nothing ('')
    const cityNoAccents = q.normalize('NFD').replace(/[\u0300-\u036f]/g, '');
    // find distance between two globe coordinates, factoring in roundness. return number in km
    const distanceFormula = findDistance
        ? client_1.Prisma.sql `
             6371 * 2 * ASIN(
               SQRT(
                 POWER(SIN(RADIANS((${latitude}::float - latitude::float) / 2)), 2) +
                 COS(RADIANS(latitude::float)) * COS(RADIANS(${latitude}::float)) *
                 POWER(SIN(RADIANS((${longitude}::float - longitude::float) / 2)), 2)
               )
             ) 
             
             `
        : client_1.Prisma.empty;
    // note that SIMILARITY is a function that compares strings and outputs a number, 1 for an identical match
    try {
        // prisma.sql enables conditional rendering in an SQL query and properly formats SQL syntax
        const suggestions = yield prisma.$queryRaw `
      SELECT
        name,
        "fullName",
        latitude,
        longitude,
        ${findDistance
            ? client_1.Prisma.sql `
                      PERCENT_RANK() OVER (ORDER BY( POWER(${distanceFormula},-1) 
                        * SIMILARITY("ascii", ${cityNoAccents})  ))`
            : client_1.Prisma.sql `SIMILARITY("ascii", ${cityNoAccents})`}
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
    `;
        if (suggestions.length > 0) {
            res.status(200).json({
                message: 'Retrieval successful',
                suggestions: suggestions,
            });
        }
        else {
            res.status(404).json({
                status: 404,
                message: `Database successfully queried, but no data matches the string "${q}"`,
                suggestions: suggestions,
            });
        }
    }
    catch (err) {
        console.log(err);
        res.status(500).json({ message: err });
    }
});
exports.findCities = findCities;
