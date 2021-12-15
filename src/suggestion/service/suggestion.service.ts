import {Injectable, Logger, OnModuleInit} from '@nestjs/common';
import {CityEntity} from "../entity/city.entity";
import {getConnection, Repository} from "typeorm";
import {InjectRepository} from "@nestjs/typeorm";
import {CityDto} from 'src/suggestion/dto/city.dto';
import * as fs from 'fs';

const parse = require('csv-parse/lib/sync');

@Injectable()
export class SuggestionService implements OnModuleInit {

    private readonly csvPath = './data/cities_canada-usa.tsv';
    private readonly citiesNameSql = './resources/sql/cities-name.sql';
    private readonly citiesCoordSql = './resources/sql/cities-GEO.sql';
    private readonly logger = new Logger(SuggestionService.name);
    private readonly encoding = 'utf-8';

    constructor (
        @InjectRepository(CityEntity)
        private suggestionRepository: Repository<CityEntity>
    ) {}

    /**
     * Initialization of DB's content
     */
    onModuleInit() {
        this.logger.log('Loading all data to Postgres');

        getConnection()
            .createQueryBuilder()
            .delete()
            .from(CityEntity).execute()

        // Parse CSV to cities
        const cities = this.parseCSV();

        // Load Data to DB
        this.loadDataToDb(cities);

        this.logger.log('Data loaded to DB');
    }


    /**
     * Load data to Postgres
     * @param cities
     * @private
     */
    private loadDataToDb(cities) {
        // Load all data to Postgres DB
        cities.forEach((city) => {
            const cityDto = this.buildCityDto(city.name, city.lat, city.long, city.ascii, city.admin1, city.country, city.population);
            if (cityDto.population >= 5000) {
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
            quote: null
        });
    }

    /**
     *
     * Retrieve cities in DB based in params, returns array of cities
     * @param q
     * @param latitude
     * @param longitude
     */
    async findCitiesWithParams(q: string, latitude: string, longitude: string): Promise<CityDto[]> {
        this.logger.log('Find cities with params : keyword : ' + q + ', latitude : ' + latitude + ', longitude : ' + longitude);
        let promise;
        if (longitude && latitude) {
            this.logger.log('Starting finding cities with geolocation');
            const citiesCoordonnees = fs.readFileSync(this.citiesCoordSql, {encoding: this.encoding});
            promise = await getConnection().query(citiesCoordonnees, [q, longitude, latitude]);
        } else {
            this.logger.log('Starting finding cities with keyword');
            const citiesNameScript = fs.readFileSync(this.citiesNameSql, {encoding: this.encoding});
            promise = await getConnection().query(citiesNameScript, [q]);
        }
        return promise;
    }

    /**
     * Retrieve all cities in DB, returns array of cities
     */
    findCities(): Promise<CityDto[]> {
        this.logger.log('Find all cities');
        return this.suggestionRepository.find();
    }

    /**
     * Create a new city in DB
     * @param cityDto
     */
    create(cityDto: CityDto): Promise<CityDto> {
        this.logger.log('Save new city to DB : ' + cityDto.name);
        return this.suggestionRepository.save(cityDto);
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
            name: name,
            lat: latitude,
            long: longitude,
            ascii: ascii,
            admin1: admin1,
            country: country,
            population: population,
            score: 0
        };
        return city;
    }
}
