import { ICitiesDataFileRow } from "models/interfaces/ICitiesDataFileRow"
import { Column, Entity, JoinColumn, ManyToOne, PrimaryGeneratedColumn } from "typeorm"
import { Country } from "./Country"
import { Province } from "./Province"

@Entity("cities")
export class City {
    
    @PrimaryGeneratedColumn()
    id: number

    @Column()
    geonameId: number
    
    @Column({length: 200})
    name: string
    
    @Column({length: 200})    
    asciiName: string
    
    @Column({length: 5000})
    alternateNames: string
    
    @Column()
    latitude: string
    
    @Column()
    longitude: string
    
    @Column({length: 1})
    featureClass: string
    
    @Column({length: 10})
    featureCode: string
    
    @ManyToOne(type => Country)
    @JoinColumn({ referencedColumnName: "isoCode" })
    country: Country;

    @Column({length: 60})
    cc2: string
    
    @ManyToOne(type => Province)
    @JoinColumn({ referencedColumnName: "code" })
    province: Province;
    
    @Column({length: 80})
    admin2Code: string
    
    @Column({length: 20})
    admin3Code: string
    
    @Column({length: 20})
    admin4Code: string
    
    @Column()
    population: number
    
    @Column()
    elevation: number
    
    @Column()
    dem: string
    
    @Column({length: 40})
    timezone: string
    
    @Column()
    modificationDate: Date

    public FromDataRow(dataRow: ICitiesDataFileRow) {
        this.geonameId = dataRow.id
        this.name = dataRow.name
        this.asciiName = dataRow.ascii
        this.alternateNames = dataRow.alt_name
        this.latitude = dataRow.lat
        this.longitude = dataRow.long
        this.featureClass = dataRow.feat_class
        this.featureCode = dataRow.feat_code
        this.country = new Country().FromInputObject({isoCode: dataRow.country})
        this.cc2 = dataRow.cc2
        this.province = new Province().FromInputObject({code: `${dataRow.country}:${dataRow.admin1}`})
        this.admin2Code = dataRow.admin2
        this.admin3Code = dataRow.admin3
        this.admin4Code = dataRow.admin4
        this.population = Number(dataRow.population)
        this.elevation = Number(dataRow.elevation)
        this.dem = dataRow.dem
        this.timezone = dataRow.tz
        this.modificationDate = new Date(dataRow.modified_at)

        return this
    }

}