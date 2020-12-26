import {
  IsInt,
  IsLatitude,
  IsLongitude,
  IsOptional,
  IsString,
  Max,
  Min,
  ValidateIf,
} from 'class-validator';
import { Transform } from 'class-transformer';

export class SuggestionRequest {
  @IsString()
  q: string;

  @IsOptional()
  @IsLatitude()
  @Transform(Number)
  latitude?: number;

  @ValidateIf((r) => !!r.latitude)
  @IsLongitude()
  @Transform(Number)
  longitude?: number;

  @IsOptional()
  @IsInt()
  @Transform(Number)
  @Min(1)
  @Max(20)
  limit?: number;
}
