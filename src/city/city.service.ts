import {Injectable, Logger} from '@nestjs/common';
import {InjectRepository} from '@nestjs/typeorm';
import {getConnection, Repository} from 'typeorm';
import * as fs from 'fs';
import {CityDto} from './../city/city.dto';
import {CityEntity} from '../city/city.entity';
import parse = require('csv-parse/lib/sync');

@Injectable()
export class CityService {
    private readonly csvPath = './resources/data/cities_canada-usa.tsv';
    private readonly logger = new Logger(CityService.name);

    private readonly citiesCoordonatesSql;
    private readonly citiesNameSql;

    constructor(
        @InjectRepository(CityEntity)
        private cityRepository: Repository<CityEntity>,
    ) {
        this.citiesCoordonatesSql = fs.readFileSync('./resources/sql/cities-geo.sql', {encoding: 'utf-8'});
        this.citiesNameSql = fs.readFileSync('./resources/sql/cities-name.sql', {encoding: 'utf-8'});
    }

    /**
     * Initialization of DB's content
     */
    seedDatabase() {
        this.logger.log('Loading all data to Postgres');

        // First we delete all data into our table
        getConnection()
            .createQueryBuilder()
            .delete()
            .from(CityEntity).execute();

        // Parse CSV to cities
        const cities = this.parseCSV();

        // Load Data to DB
        this.loadDataToDb(cities);

        this.logger.log('Data loaded to DB');
    }

    /**
     *
     * Retrieve cities in DB based in params, returns array of cities
     * @param q
     * @param latitude
     * @param longitude
     */
    async findCitiesWithParams(q: string, latitude: string, longitude: string): Promise<CityDto[]> {
        let cities: CityDto[];
        if (longitude && latitude) {
            this.logger.log('Starting finding cities with geolocation');
            cities = await this.cityRepository.query(this.citiesCoordonatesSql, [q, longitude, latitude]);
        } else {
            this.logger.log('Starting finding cities with keyword');
            cities = await this.cityRepository.query(this.citiesNameSql, [q]);
        }
        return cities;
    }

    /**
     * Create a new city in DB
     * @param cityDto
     */
    create(cityDto: CityDto): Promise<CityDto> {
        this.logger.log('Save new city to DB : ' + cityDto.name);
        return this.cityRepository.save(cityDto);
    }

    /**
     * Build a city DTO object based on data from CSV
     * @param name
     * @param latitude
     * @param longitude
     * @param ascii
     * @param admin1
     * @param country
     * @param population
     */
    buildCityDto(name: string, latitude: string, longitude: string, ascii: string, admin1: string, country: string, population: number): CityDto {
        const city: CityDto = {
            name,
            lat: latitude,
            long: longitude,
            ascii,
            admin1,
            country,
            population,
            score: 0,
        };
        return city;
    }

    /**
     * Load data to Postgres
     * Only with population above 5000 and cities in US or Canada
     * @param cities
     * @private
     */
    private loadDataToDb(cities: CityDto[]) {
        // Load all data to Postgres DB
        cities.forEach((city) => {
            const cityDto = this.buildCityDto(city.name, city.lat, city.long, city.ascii, city.admin1, city.country, city.population);
            if (cityDto.population >= 5000 && (cityDto.country.normalize() === 'CA'.normalize()
                || cityDto.country.normalize() === 'US'.normalize())) {
                this.create(cityDto);
            }
        });
    }

    /**
     * ParseCSV to cities
     * @private
     */
    private parseCSV() {
        this.logger.log('Read CSV file ' + this.csvPath);
        return parse(fs.readFileSync(this.csvPath), {
            delimiter: '\t',
            escape: '\\',
            columns: true,
            quote: null,
        });
    }
}
