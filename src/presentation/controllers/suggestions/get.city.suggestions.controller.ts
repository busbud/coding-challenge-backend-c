import { Controller, Get, HttpStatus, Query, UsePipes } from '@nestjs/common';
import { ApiOperation, ApiQuery, ApiResponse, ApiTags } from '@nestjs/swagger';

import { JoiValidationPipe } from '@infrastructure/rest/pipes/joi.validation.pipe';
import { suggestionsQueryJoiSchema } from '@infrastructure/validators/suggestions.joi.schema';
import { ICitySuggestions } from '@domain/interfaces/suggestions/i.city.suggestions';
import { GetCitySuggestionsUseCase } from '@application/usecases/suggestions/get.city.suggestions.use.case';

@ApiTags(`suggestions`)
@Controller('/api/v1/suggestions')
export class GetCitySuggestionsController {
  constructor(private readonly getCitySuggestionsUseCase: GetCitySuggestionsUseCase) {}
  @ApiQuery({ name: 'q', type: String })
  @ApiQuery({ name: 'latitude', type: String, required: false })
  @ApiQuery({ name: 'longitude', type: String, required: false })
  @ApiOperation({ summary: 'get suggestions by query strings' })
  @ApiResponse({
    status: HttpStatus.OK,
    description: `<b>OK</b>`,
  })
  @Get('/')
  @UsePipes(new JoiValidationPipe(suggestionsQueryJoiSchema))
  async execute(@Query() query): Promise<ICitySuggestions[]> {
    return await this.getCitySuggestionsUseCase.execute(query);
  }
}
