import { Table, Column, Model, DataType } from 'sequelize-typescript'

@Table({timestamps: false})
export class City extends Model {
  @Column({ type: DataType.INTEGER })
  geonameId!: number

  @Column({ type: DataType.STRING })
  name!: string

  @Column({ type: DataType.DATE })
  modifiedAt: Date;
}