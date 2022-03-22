import "reflect-metadata";
import { ObjectType, Field, Int, Float } from "@nestjs/graphql";

//create cities and cities input object types
@ObjectType()
export class cities {
  @Field((type) => Int)
  id: number;

  @Field((type) => String)
  name: string;

  @Field((type) => String)
  ascii_name: string;

  @Field((type) => String)
  alternate_name: string;

  @Field((type) => Float)
  latitude: Float32Array;

  @Field((type) => Float)
  longitude: Float32Array;

  @Field((type) => String)
  country: string;

  @Field((type) => String)
  state: string;

  @Field((type) => Date)
  createdAt: Date;

  @Field((type) => Date)
  updatedAt: Date;
}

@ObjectType()
export class citiesRes {
  @Field((type) => Int)
  id: number;

  @Field((type) => String)
  name: string;

  @Field((type) => Float)
  latitude: Float32Array;

  @Field((type) => Float)
  longitude: Float32Array;

  @Field((type) => Float)
  distance: Float32Array;

  @Field((type) => Float)
  score: Float32Array;
}
