import { Table, Column, Model, DataType } from 'sequelize-typescript'
import CityDTO from '../value-object/city-dto';

@Table({timestamps: false})
export class City extends Model {
  @Column({ type: DataType.INTEGER })
  public geonameId: number

  @Column({ type: DataType.STRING })
  public name: string

  @Column({ type: DataType.JSON })
  public altNames: string[];

  @Column({ type: DataType.FLOAT })
  public latitude: number;
  
  @Column({ type: DataType.FLOAT })
  public longitude: number;
  
  @Column({ type: DataType.STRING })
  public featClass: string;

  @Column({ type: DataType.STRING })
  public featCode: string;
  
  @Column({ type: DataType.STRING })
  public country: string;
  
  @Column({ type: DataType.JSON })
  public cc2: string[];

  @Column({ type: DataType.STRING })
  public admin1: string;

  @Column({ type: DataType.STRING })
  public admin2: string;

  @Column({ type: DataType.STRING })
  public admin3: string;

  @Column({ type: DataType.STRING })
  public admin4: string;

  @Column({ type: DataType.INTEGER })
  public population: number;

  @Column({ type: DataType.INTEGER })
  public elevation: number;

  @Column({ type: DataType.STRING })
  public dem: string;

  @Column({ type: DataType.STRING })
  public tz: string;

  @Column({ type: DataType.DATE })
  modifiedAt: Date;

  static createFromDTO(dto: CityDTO): City {
    return new this({
      geonameId: dto.geonameid,
      name: dto.name,
      altNames: dto.alt_name,
      latitude: dto.lat,
      longitude: dto.long,
      featClass: dto.feat_class,
      featCode: dto.feat_code,
      country: dto.country,
      cc2: dto.cc2,
      admin1: dto.admin1,
      admin2: dto.admin2,
      admin3: dto.admin3,
      admin4: dto.admin4,
      population: dto.population,
      elevation: dto.elevation,
      dem: dto.dem,
      tz: dto.tz,
      modifiedAt: dto.modified_at
    });
  }

  getCompleteName(): string {
    return this.name + ', ' + this.country;
  }
}
