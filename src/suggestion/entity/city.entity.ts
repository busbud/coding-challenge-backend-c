import {Column, Entity, Index, PrimaryGeneratedColumn} from "typeorm";

@Entity('city')
@Index(["name"])
export class CityEntity {

    @PrimaryGeneratedColumn()
    id: number;

    @Column({nullable: false})
    name: string;

    @Column({name: 'ascii', nullable: true})
    ascii: string;

    @Column({type: 'real', name: 'lat', nullable: true})
    lat: string;

    @Column({type: 'real', name: 'long', nullable: true})
    long: string;

    @Column({name: 'country', nullable: true})
    country: string;

    @Column({name: 'admin1', nullable: true})
    admin1: string;

    @Column({name: 'population', nullable: true})
    population: number;

    @Column({type: 'geography', name: 'localisation', nullable: true})
    localisation: string;

    feat_class: string;

    feat_code: string;

    score: number;

    lastName: string;

    alt_name: string;

    cc2: string;

    admin2: string;

    admin3: string;

    admin4: string;

    elevation: number;

    dem: number;

    tz: string;
}
