import {
  Body,
  Controller,
  Delete,
  Get,
  Param,
  Post,
  Put,
  Query,
} from '@nestjs/common';
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

  @Post()
  create(@Body() city: City): Observable<void> {
    return this.service.addCity(city);
  }

  @Put(':id')
  update(
    @Param('id') id: string,
    @Body() city: Omit<City, 'id'>,
  ): Observable<void> {
    return this.service.addCity({ id, ...city });
  }

  @Delete(':id')
  delete(@Param('id') id: string): Observable<void> {
    return this.service.removeCity(id);
  }
}
