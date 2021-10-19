import { Column, Entity, PrimaryColumn } from "typeorm"

@Entity("countries")
export class Country {
    
    @PrimaryColumn({length: 2})
    isoCode: string
    
    @Column({length: 200})
    countryName: string

    public FromInputObject(inputObject: Partial<Country>): Country {
        return Object.assign(this, inputObject) as Country
    }

}