import { Controller, Get, Query } from '@nestjs/common';
import { CitiesService } from './cities.service';
import { from, Observable } from 'rxjs';
import { City } from './interfaces/city';
import { toArray } from 'rxjs/operators';

@Controller('cities')
export class CitiesController {
  constructor(private service: CitiesService) {}

  @Get()
  findAll(@Query('q') query?: string): Observable<City[]> {
    const cities$ = query ? this.service.queryCities({ query }) : from([]);
    return cities$.pipe(toArray());
  }
}
