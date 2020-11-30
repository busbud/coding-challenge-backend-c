import { CityDao } from '../dao/CityDao';
import { CityData } from '../models/CityData';

const MIN_POPULATION = 5000;
const ZONE_RESTICTION_LIST = ['CA', 'US'];

enum CanadianProvinces {
    AB = 1,
    BC = 2,
    MB = 3,
    NB = 4,
    NL = 5,
    NS = 7,
    ON = 8,
    PE = 9,
    QC = 10,
    SK = 11,
    YT = 12,
    NT = 13,
    NU = 14,
}

export class CityService {
    private dao: CityDao = new CityDao();
    private static instance: CityService;

    constructor() {
        if (CityService.instance) {
            return CityService.instance;
        }
        CityService.instance = this;
    }
    public read(): CityData[] {
        const eligibleCities: CityData[] = [];
        this.dao.read().forEach((data: { [key: string]: string }) => {
            if (Number(data.population) > MIN_POPULATION && ZONE_RESTICTION_LIST.includes(data.country)) {
                (data.alt_name as any) = data.alt_name ? data.alt_name.split(',') : [];
                if (data.country === ZONE_RESTICTION_LIST[0] && Number(data.admin1)) {
                    data.admin1 = CanadianProvinces[Number(data.admin1)];
                }
                eligibleCities.push(new CityData().parseToModel(data));
            }
        });
        return eligibleCities;
    }
}