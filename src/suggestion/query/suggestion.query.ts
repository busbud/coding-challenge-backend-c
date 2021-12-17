import {ApiProperty, ApiPropertyOptional} from '@nestjs/swagger';
import {IsOptional, IsString, Validate} from 'class-validator';
import {LatitudeValidator} from '../validator/LatitudeValidator';
import {LongitudeValidator} from '../validator/LongitudeValidator';

export class SuggestionQuery {
    @ApiProperty({
        description: 'The name of a city',
        type: String,
    })
    @IsString()
    q: string;

    @ApiProperty({
        description: 'The latitude coordinates',
        type: String,
    })
    @Validate(LatitudeValidator)
    @ApiPropertyOptional()
    @IsOptional()
    latitude: string;

    @ApiProperty({
        description: 'The longitude coordinates',
        type: String,
    })
    @Validate(LongitudeValidator)
    @ApiPropertyOptional()
    @IsOptional()
    longitude: string;
}
