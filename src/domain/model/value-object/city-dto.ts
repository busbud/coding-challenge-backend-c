/* eslint-disable */
export default class CityDTO {
    public geonameid: number;
    public name: string;
    public ascii: string;
    public alt_name: string[];
    public lat: number;
    public long: number;
    public feat_class: string;
    public feat_code: string;
    public country: string;
    public cc2: string[];
    public admin1: string;
    public admin2: string;
    public admin3: string;
    public admin4: string;
    public population: number;
    public elevation: number;
    public dem: string;
    public tz: string;
    public modified_at: Date;

    public constructor(record: any) {
        this.geonameid = Number(record.id);
        this.name = record.name;
        this.ascii = record.ascii;
        this.alt_name = record.alt_name ? record.alt_name.split(',') : [];
        this.lat = Number(record.lat);
        this.long = Number(record.long);
        this.feat_class = record.feat_class;
        this.feat_code = record.feat_code;
        this.country = record.country;
        this.cc2 = record.cc2 ? record.cc2.split(',') : [];
        this.admin1 = record.admin1;
        this.admin2 = record.admin2;
        this.admin3 = record.admin3;
        this.admin4 = record.admin4;
        this.population = Number(record.population);
        this.elevation = Number(record.elevation);
        this.dem = record.dem;
        this.tz = record.tz;
        this.modified_at = new Date(record.modified_at);
    }
}
