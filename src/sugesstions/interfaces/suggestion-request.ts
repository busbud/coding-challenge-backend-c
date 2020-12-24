import {
  IsLatitude,
  IsLongitude,
  IsOptional,
  IsString,
  ValidateIf,
} from 'class-validator';

export class SuggestionRequest {
  @IsString()
  q: string;

  @IsOptional()
  @IsLatitude()
  latitude?: number;

  @ValidateIf((r) => !!r.latitude)
  @IsLongitude()
  longitude?: number;
}
