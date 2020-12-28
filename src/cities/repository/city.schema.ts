import { Prop, Schema, SchemaFactory } from '@nestjs/mongoose';
import { Document } from 'mongoose';
import { City as ICity } from '../interfaces/city';
import { Location } from '../../location';

export type CityDocument = City & Document;

@Schema()
export class LocationSchema implements Location {
  @Prop()
  lat: number;
  @Prop()
  lng: number;
}

@Schema()
export class City implements ICity {
  @Prop()
  name: string;
  @Prop()
  alt_name: string;
  @Prop()
  country: string;
  @Prop()
  geohash: string;
  @Prop()
  id: string;
  @Prop({ type: LocationSchema })
  location: Location;
  @Prop()
  normalized_name: string;
  @Prop()
  population: number;
  @Prop()
  state: string;
}

export const CitySchema = SchemaFactory.createForClass(City);
