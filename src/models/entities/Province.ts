import { Column, Entity, PrimaryColumn, PrimaryGeneratedColumn, Unique } from "typeorm"

@Entity("provinces")
export class Province {
    
    @PrimaryColumn({length: 5})
    code: string

    @Column({length: 2})
    provinceCode: string
    
    @Column({length: 200})
    provinceName: string

    public FromInputObject(inputObject: Partial<Province>): Province {
        return Object.assign(this, inputObject) as Province
    }
}