import {IsOptional, IsString, Validate} from "class-validator";
import {LatitudeValidator} from '../validator/LatitudeValidator';
import {LongitudeValidator} from "../validator/LongitudeValidator";

export class SuggestionQuery {

    @IsString()
    q: string;

    @Validate(LatitudeValidator)
    @IsOptional()
    latitude: string;

    @Validate(LongitudeValidator)
    @IsOptional()
    longitude: string;
}
