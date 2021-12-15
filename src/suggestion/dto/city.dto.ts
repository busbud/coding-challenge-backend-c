import {IsNumber, IsString} from "class-validator";

export class CityDto {
    @IsString()
    name: string;

    @IsString()
    ascii: string;

    @IsString()
    admin1: string;

    @IsString()
    country: string;

    @IsString()
    lat: string;

    @IsString()
    long: string;

    @IsNumber()
    score: number;

    @IsNumber()
    population: number;
}

