import * as fs from 'fs'
import { parse } from '@fast-csv/parse'
import { City } from 'models/entities/City'
import { ICitiesDataFileRow } from 'models/interfaces/ICitiesDataFileRow'
import { ICountriesDataFileRow } from 'models/interfaces/ICountriesDataFileRow'
import { IProvincesDataFileRow } from 'models/interfaces/IProvincesDataFileRow'
import { getCustomRepository } from 'typeorm'
import { CityRepository } from 'repositories/CityRepository'
import { Country } from 'models/entities/Country'
import { Province } from 'models/entities/Province'
import { CountryRepository } from 'repositories/CountryRepository'
import { ProvinceRepository } from 'repositories/ProvinceRepository'


export abstract class DatabaseServices {

    // Checks database data and populates tables when data not present
    public static async CheckAvailableData() {

        const cityRepository = getCustomRepository(CityRepository)
        const countryRepository = getCustomRepository(CountryRepository)
        const provinceRepository = getCustomRepository(ProvinceRepository)

        const citieInDatabase = await cityRepository.getCitiesCount()

        // Do not run database population when data is already present
        if(citieInDatabase > 0)
            return

        const countriesDataFile = process.env.DATAFILE_COUNTRIES
        const provincesDataFile = process.env.DATAFILE_PROVINCES
        const citiesDataFile = process.env.DATAFILE_CITIES

        const countryParser = parse({
            delimiter: '\t', 
            quote: '', 
            escape: '', 
            headers: ["isoCode","countryName"]})
            .on('error', (error) => console.error(error))
            .on('data', async (row: ICountriesDataFileRow) => {
                await countryRepository.save(new Country().FromInputObject(row))
            })
            .on('end', (total) => console.log(`Total countries: ${total}`))

        const provinceParser = parse({
            delimiter: '\t', 
            quote: '', 
            escape: '', 
            headers: ["code","provinceCode","provinceName"]})
            .on('error', (error) => console.error(error))
            .on('data', async (row: IProvincesDataFileRow) => {
                await provinceRepository.save(new Province().FromInputObject(row))
            })
            .on('end', (total) => console.log(`Total provinces: ${total}`))

        const citiesParser = parse({
            delimiter: '\t', 
            quote: '', 
            escape: '', 
            headers: ['id','name','ascii','alt_name','lat','long','feat_class','feat_code','country','cc2','admin1','admin2','admin3','admin4','population','elevation','dem','tz','modified_at']})
            .on('error', (error) => console.error(error))
            .on('data', async (row: ICitiesDataFileRow) => {
                await cityRepository.save(new City().FromDataRow(row))
            })
            .on('end', (total) => console.log(`Total cities: ${total}`))

        fs.createReadStream(countriesDataFile)
            .pipe(countryParser)

        fs.createReadStream(provincesDataFile)
            .pipe(provinceParser)

        fs.createReadStream(citiesDataFile)
            .pipe(citiesParser)
    }
}
